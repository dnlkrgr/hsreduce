module Parser.Parser (getPragmas, parse) where

import Control.Applicative (Alternative ((<|>), many, some))
import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (fromRight, isRight)
import Data.Functor (void)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import DynFlags (xopt_set)
import GHC
    ( LoadHowMuch (LoadAllTargets),
      gcatch,
      getModSummary,
      getSessionDynFlags,
      guessTarget,
      load,
      mkModuleName,
      parseDynamicFlags,
      parseModule,
      runGhc,
      setSessionDynFlags,
      setTargets,
      typecheckModule,
    )
import GHC.Paths (libdir)
import qualified Language.Haskell.GHC.ExactPrint as EP
import Outputable (ppr, showSDocUnsafe)
import Path (Abs, File, Path, fromAbsFile)
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
    ( Parser,
      Pragma (..),
      RState (RState),
      emptyStats,
      pragma2Extension,
      showExtension,
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
    banner $ "Parsing: " <> fromAbsFile fileName

    let filePath = fromAbsFile fileName
    p <- EP.parseModule filePath >>= \case
        Left e -> error . showSDocUnsafe $ ppr e
        Right p -> return $ snd p

    mt <-
        do
            modName <-
                ( \case
                      Left _ -> "Main"
                      Right t -> t
                    )
                    . getModName
                    <$> TIO.readFile filePath

            extensions <- catMaybes . map pragma2Extension <$> getPragmas fileName

            runGhc (Just libdir) $ do
                dflags <- flip (L.foldl' xopt_set) extensions . (\(a, _, _) -> a) <$> (flip parseDynamicFlags [] =<< getSessionDynFlags)
                _ <- setSessionDynFlags dflags

                target <- guessTarget (fromAbsFile fileName) Nothing
                setTargets [target]
                _ <- load LoadAllTargets
                modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName

                pm <- parseModule modSum

                gcatch (Just <$> typecheckModule pm) (\(_ :: SomeException) -> return Nothing)

    prags <- getPragmas fileName

    return $ RState prags p False emptyStats 0 mt

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

banner :: MonadIO m => String -> m ()
banner s = liftIO $ putStrLn $ "\n" <> s' <> s <> s'
    where
        n = 80 - length s
        s' = replicate (div n 2) '='