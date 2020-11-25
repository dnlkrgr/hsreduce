module Parser.Parser where

import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import StringBuffer
import FastString
import SrcLoc
import Lexer
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MC
import Control.Applicative (Alternative ((<|>), many, some))
import Data.Either (fromRight, isRight)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import qualified DynFlags as GHC
import GHC
    ( getModSummary,
      guessTarget,
      parseModule,
      runGhc,
      setSessionDynFlags,
      setTargets,
      typecheckModule,
      load,
      getSessionDynFlags,
      mkModuleName,
      DynFlags,
      GeneralFlag(Opt_KeepRawTokenStream),
      gcatch,
      LoadHowMuch(LoadAllTargets),
      GhcMonad(getSession) )
import GHC.Paths (libdir)
import qualified HeaderInfo as GHC
import qualified Language.Haskell.GHC.ExactPrint as EP
import Outputable (ppr, showSDocUnsafe)
import Path (Abs, File, Path, fromAbsFile)
import qualified StringBuffer as GHC
import Text.Megaparsec.Char
    ( alphaNumChar,
      char,
      letterChar,
      space,
      string,
      upperChar,
    )
import Util.Types
    

-- TODO: how to handle Safe vs. Trustworthy?
getPragmas :: Path Abs File -> IO [Pragma]
getPragmas f =
    filter ((/= "Safe") . showExtension)
        . concat
        . fromRight []
        . sequence
        . filter isRight
        . map (MP.parse pragmasP (fromAbsFile f))
        . T.lines
        <$> TIO.readFile (fromAbsFile f)

pragmasP :: Parser [Pragma]
pragmasP = do
    _ <- string "{-#"
    space
    f <- pragmaType
    space
    n <- T.pack <$> some letterChar
    ns <- many (char ',' >> space >> T.pack <$> some letterChar)
    space
    _ <- string "#-}"
    return . map f $ n : ns

pragmaType :: Parser (T.Text -> Pragma)
pragmaType =
    ((string "language" <|> string "LANGUAGE") >> return Language)
        <|> ((string "OPTIONS_GHC" <|> string "options_ghc") >> return OptionsGhc)
        <|> ((string "INCLUDE" <|> string "include") >> return Include)

parse :: Path Abs File -> IO RState
parse fileName = do
    -- banner $ "Parsing: " <> fromAbsFile fileName

    let filePath = fromAbsFile fileName
    p <- EP.parseModule filePath >>= \case
        Left e -> error . showSDocUnsafe $ ppr e
        Right p -> return $ snd p

    modName <-
        ( \case
              Left _ -> "Main"
              Right t -> t
            )
            . getModName
            <$> TIO.readFile filePath

    prags <- getPragmas fileName

    ( runGhc (Just libdir) $ do
          -- dflags <- flip (L.foldl' xopt_set) extensions . (\(a, _, _) -> a) <$> (flip parseDynamicFlags [] =<< getSessionDynFlags)
          -- _ <- setSessionDynFlags dflags
          df <- initDynFlagsPure (fromAbsFile fileName)

          target <- guessTarget (fromAbsFile fileName) Nothing
          setTargets [target]
          _ <- load LoadAllTargets
          modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName
          -- liftIO $ print $ "modName: " <> modName

          pm <- parseModule modSum
          hEnv <- getSession

          gcatch
              ( do
                    tm <- typecheckModule pm
                    return . Right $ (tm, hEnv, df)
              )
              (\(_ :: SomeException) -> return $ Left df)
        )
        >>= \case
            Left dynFlags -> return $ ParsedState prags p False emptyStats 0 dynFlags
            Right (mt, hEnv, df) -> return $ TypecheckedState prags p False emptyStats 0 mt hEnv df

initDynFlagsPure :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlagsPure fp = do
    s <- liftIO $ T.unpack <$> TIO.readFile fp
    -- I was told we could get away with using the unsafeGlobalDynFlags.
    -- as long as `parseDynamicFilePragma` is impure there seems to be
    -- no reason to use it.
    dflags0 <- GHC.getSessionDynFlags
    let pragmaInfo = GHC.getOptions dflags0 (GHC.stringToStringBuffer $ s) fp
    (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 pragmaInfo
    -- Turn this on last to avoid T10942
    let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
    -- Prevent parsing of .ghc.environment.* "package environment files"
    (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine dflags2 []
    -- (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine dflags2 [GHC.noLoc "-hide-all-packages"]
    _ <- GHC.setSessionDynFlags dflags3
    return dflags3

runParser :: DynFlags -> P a -> String -> ParseResult a
runParser flags parser str = unP parser parseState
    where
      location = mkRealSrcLoc (mkFastString "<interactive>") 1 1
      buffer = stringToStringBuffer str
      parseState = mkPState flags buffer location

countTokensOfTestCases :: IO ()
countTokensOfTestCases = do
    l1 <- traverse (countTokens . (\s -> "../hsreduce-test-cases/" <> s <> "/Bug.hs")) testCases
    l2 <- traverse (countTokens . (\s -> "../hsreduce-test-cases/" <> s <> "/Bug_creduce.hs")) testCases
    l3 <- traverse (countTokens . (\s -> "../hsreduce-test-cases/" <> s <> "/Bug_hsreduce.hs")) testCases
    forM_ (zip testCases $ zip3 l1 l2 l3) print
    where
        testCases = ["ticket14040", "ticket14270", "ticket14779", "ticket14827", "ticket15696_1", "ticket15696_2", "ticket16979", "ticket18098", "ticket18140_1", "ticket18140_2", "ticket8763"]

countTokens :: FilePath -> IO (Maybe Int)
countTokens fp = do
    try (runGhc (Just libdir) (initDynFlagsPure fp)) >>= \case
        Left (_ :: SomeException) -> pure Nothing
        Right flags -> do
            str <- TIO.readFile fp
            pure $ countTokensHelper flags $ T.unpack str

countTokensHelper :: DynFlags -> String -> Maybe Int
countTokensHelper flags str = do
    let buffer = stringToStringBuffer str
    case lexTokenStream buffer location flags of
        POk _ toks -> Just $ length toks
        _ -> Nothing
    where 
        location = mkRealSrcLoc (mkFastString "<interactive>") 1 1

getModName :: T.Text -> Either (MP.ParseErrorBundle T.Text Void) T.Text
getModName = fmap T.concat . sequence . filter isRight . map (MP.parse getModName' "") . T.lines

getModName' :: Parser T.Text
getModName' = do
    _ <- space
    _ <- string "module"
    _ <- space
    n <- T.concat <$> some nameP
    _ <- space
    return n

nameP :: Parser T.Text
nameP = T.cons <$> char '.' <*> nameP' <|> nameP'

nameP' :: Parser T.Text
nameP' = do
    c <- upperChar
    cs <- many alphaNumChar
    return . T.pack $ c : cs

useP :: MP.Parsec e s a -> s -> Either (MP.ParseErrorBundle s e) a
useP = flip MP.parse ""

notInScopeP :: Parser T.Text
notInScopeP = do
    _ <- MP.chunk "Not in scope:"
    somethingInTicksP

perhapsYouMeantP :: Parser T.Text
perhapsYouMeantP = do
    MP.try
        ( do
              _ <- MP.chunk "Not in scope:"
              _ <-  somethingInTicksP
              _ <- MC.char '\8217'
              pure ()
        )
        <|> MP.try (dotsP "Variable not in scope:")
        <|> dotsP "Data constructor not in scope:"
    MC.space
    _ <- (MP.try (MP.chunk "Perhaps you meant") <|> MP.chunk "Perhaps you meant one of these:")
    somethingInTicksP

somethingInTicksP :: Parser T.Text
somethingInTicksP = do
    _ <- MP.some (MP.satisfy (/= '\8216'))
    _ <- MC.char '\8216'
    T.pack <$> MP.some (MP.satisfy (/= '\8217'))

dotsP :: T.Text -> Parser ()
dotsP s = do
    _ <- MC.char '\8226'
    MC.space
    _ <- MP.chunk s
    _ <- MP.some (MP.satisfy (/= '\8226'))
    _ <- MC.char '\8226'
    pure ()

removeUseOfHidden :: T.Text -> T.Text
removeUseOfHidden s
    | length components > 2 =
        removeInternal init (T.intercalate "." $ init components) <> "." <> (last components)
    | otherwise = s
    where
        components = modname2components s

removeInternal :: ([T.Text] -> [T.Text]) -> T.Text -> T.Text
removeInternal f s
    | length components > 2 = T.intercalate "." . (\wrds -> if "Internal" `elem` wrds then takeWhile (/= "Internal") wrds else f wrds) $ components
    | otherwise = s
    where
        components = modname2components s

hiddenImportP :: Parser T.Text
hiddenImportP = do
    _ <- MP.chunk "Could not load module"
    MC.space
    _ <- MC.char '‘'
    T.pack <$> MP.some (MP.satisfy (/= '’'))

noModuleNamedP :: Parser T.Text
noModuleNamedP = do
    _ <- MP.chunk "Not in scope:"
    MC.space
    go
    _ <- MC.char '‘'
    T.pack <$> MP.some (MP.satisfy (/= '’'))
    where
        go = do
            MP.try (MP.chunk "No module named" >> MC.space)
                <|> do
                    _ <- MP.some (MP.satisfy (/= '\n'))
                    MC.space
                    go

importedFromP :: Parser T.Text
importedFromP = do
    _ <- MP.some $ MP.satisfy (/= '(')
    _ <- MC.char '('
    _ <- MP.chunk "imported"
    MC.space
    _ <- MP.chunk "from"
    MC.space
    fmap T.pack . MP.some $ MP.satisfy (/= ')')


modname2components :: T.Text -> [T.Text]
modname2components = T.words . T.map (\c -> if c == '.' then ' ' else c)
