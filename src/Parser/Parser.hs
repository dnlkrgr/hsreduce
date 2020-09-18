module Parser.Parser (getPragmas, parse) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Either
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.Haskell.GHC.ExactPrint as EP
import Outputable (ppr, showSDocUnsafe)
import Path
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
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

    prags <- getPragmas fileName

    return $ RState prags p False emptyStats 0


banner :: MonadIO m => String -> m ()
banner s = liftIO $ putStrLn $ "\n" <> s' <> s <> s'
    where
        n = 80 - length s
        s' = replicate (div n 2) '='