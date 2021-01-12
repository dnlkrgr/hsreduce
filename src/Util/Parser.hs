module Util.Parser where

import Bag
import Control.Exception
import Control.Monad.IO.Class
import StringBuffer
import FastString
import SrcLoc
import Lexer
import qualified Text.Megaparsec as MP
import Control.Applicative (Alternative ((<|>), many, some))
import Data.Either (fromRight, isRight)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import qualified DynFlags as GHC
import GHC hiding (parser)
import Parser
import GHC.Paths (libdir)
import qualified HeaderInfo as GHC
import qualified Language.Haskell.GHC.ExactPrint as EP
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
        Left e -> error $ show $ bagToList e
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

          pm <- GHC.parseModule modSum
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
    let pragmaInfo = GHC.getOptions dflags0 (GHC.stringToStringBuffer s) fp
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

quickParse :: FilePath -> IO (Maybe ParsedSource)
quickParse fp = do
    try (runGhc (Just libdir) (initDynFlagsPure fp)) >>= \case
        Left (_ :: SomeException) -> pure Nothing
        Right flags -> do
            str <- T.unpack <$> TIO.readFile fp
            case runParser flags Parser.parseModule str of
                POk _ pr -> pure $ Just pr
                _ -> pure Nothing

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
