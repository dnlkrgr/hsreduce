module Parser.Parser (getPragmas) where

import Path
import Data.Either
import Data.Functor
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Util.Types
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char

-- TODO: how to handle Safe vs. Trustworthy?
getPragmas :: Path Abs File -> IO [Pragma]
getPragmas f =
    filter ((/= "Safe") . showExtension)
  . concat
  . fromRight []
  . sequence
  . filter isRight
  . map (M.parse pragmas (fromAbsFile f))
  . T.lines
  <$> TIO.readFile (fromAbsFile f)

pragmas :: Parser [Pragma]
pragmas = do
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
