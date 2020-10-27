module Parser.Parser (getPragmas, parse) where

import Control.Applicative (Alternative ((<|>), many, some))
import Control.Exception (SomeException)
import Data.Either (fromRight, isRight)
import Data.Functor (void)
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
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
    ( alphaNumChar,
      char,
      letterChar,
      space,
      string,
      upperChar,
    )
import Util.Types
    ( Pragma(..),
      RState(RState),
      emptyStats,
      Parser,
      showExtension
      )
    

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
    void $ string "{-#"
    space
    f <- pragmaType
    space
    n <- T.pack <$> some letterChar
    ns <- many (char ',' >> space >> T.pack <$> some letterChar)
    space
    void $ string "#-}"
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
    fileContent <- readFile $ fromAbsFile fileName

    ( runGhc (Just libdir) $ do
          -- dflags <- flip (L.foldl' xopt_set) extensions . (\(a, _, _) -> a) <$> (flip parseDynamicFlags [] =<< getSessionDynFlags)
          -- _ <- setSessionDynFlags dflags
          dflags <- initDynFlagsPure (fromAbsFile fileName) fileContent

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
                    return . Just $ (tm, hEnv, dflags)
              )
              (\(_ :: SomeException) -> return Nothing)
        )
        >>= \case
            Nothing -> return $ RState prags p False emptyStats 0 Nothing Nothing Nothing
            Just (mt, hEnv, dflags) -> return $ RState prags p False emptyStats 0 (Just mt) (Just hEnv) (Just dflags)

initDynFlagsPure :: GHC.GhcMonad m => FilePath -> String -> m GHC.DynFlags
initDynFlagsPure fp s = do
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

getModName :: T.Text -> Either (MP.ParseErrorBundle T.Text Void) T.Text
getModName = fmap T.concat . sequence . filter isRight . map (MP.parse getModName' "") . T.lines

getModName' :: Parser T.Text
getModName' = do
    void space
    void $ string "module"
    void space
    n <- T.concat <$> some name
    void space
    return n

name :: Parser T.Text
name = T.cons <$> char '.' <*> name' <|> name'

name' :: Parser T.Text
name' = do
    c <- upperChar
    cs <- many alphaNumChar
    return . T.pack $ c : cs