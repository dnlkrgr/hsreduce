module Parser.Parser (parse, getPragmas, mod2DepNames) where

import System.FilePath.Posix
import Data.Either
import Data.Functor
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import "ghc" GHC
import "ghc" DynFlags
import GHC.Paths
import Util.Types
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import Data.Void

type Parser = M.Parsec Void T.Text

mod2DepNames :: [FilePath] -> [FilePath] -> FilePath -> IO [String]
mod2DepNames includeDirs srcDirs fileName = do
  let rootPath = fst $ splitFileName fileName

  mg <- runGhc (Just libdir) $ do
    dflags          <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags []
    _ <- setSessionDynFlags dflags' { includePaths = IncludeSpecs [] includeDirs, importPaths = srcDirs }

    target <- guessTarget fileName Nothing
    setTargets [target]
    _ <- load LoadAllTargets

    getModuleGraph

  return . map ((rootPath ++) . (++ ".hs") . map (\c -> if c == '.' then '/' else c) . moduleNameString . ms_mod_name) . mgModSummaries $ mg


-- TODO: collect and return all pragmas
parse :: [FilePath] -> [FilePath] -> FilePath -> IO RState
parse includeDirs srcDirs fileName = do
  banner $ "Parsing: " ++ fileName

  modName <-
    (\case
      Left _ -> "Main"
      Right t -> t)
    . getModName
    <$> TIO.readFile fileName

  print modName

  (p,t) <- runGhc (Just libdir) $ do
    dflags          <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags []
    _ <- setSessionDynFlags dflags' { includePaths = IncludeSpecs [] includeDirs, importPaths = srcDirs }

    target <- guessTarget fileName Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName

    p <- parseModule modSum
    -- TODO: catch and handle exceptions that could be thrown by typechecking
    t <- typecheckModule p
    return (p, t)

  prags <- getPragmas fileName

  return $ RState prags (parsedSource p) (renamedSource t) (Just $ typecheckedSource t)

getPragmas :: FilePath -> IO [Pragma]
getPragmas f =
        -- ([Language "DoAndIfThenElse", Language "BlockArguments", Language "DataKinds", Language "UndecidableInstances" , Language "FlexibleContexts"] ++)
        ([Language "StandaloneDeriving"] ++)
        . filter ((/= "Safe") . showExtension)
        . concat
        . fromRight []
        . sequence
        . filter isRight
        . map (M.parse pragmas f )
        . T.lines
        <$> TIO.readFile f

pragmas :: Parser [Pragma]
pragmas = do
  void $ string "{-#"
  space
  f <- pragmaType
  space
  n <- T.pack <$> some letterChar
  ns <- many (char ',' >> T.pack <$> some letterChar)
  space
  void $ string "#-}"
  return . map f $ n : ns

pragmaType :: Parser (T.Text -> Pragma)
pragmaType =
  ((string "language" <|> string "LANGUAGE") >> return Language)
  <|> ((string "OPTIONS_GHC" <|> string "options_ghc") >> return GhcOption)
  <|> ((string "INCLUDE" <|> string "include") >> return GhcOption)

-- BUG: parse error bei ListUtils, weiss noch nicht warum
getModName :: T.Text -> Either (M.ParseErrorBundle T.Text Void) T.Text
getModName = fmap T.concat . sequence . filter isRight . map (M.parse getModName' "") . T.lines

getModName' :: Parser T.Text
getModName' = do
  void space
  void $ string "module"
  void space
  n <- T.concat <$> some name
  void space
  return n

name :: Parser T.Text
name = do
    T.cons <$> char '.' <*> name'
  <|> name'

name' :: Parser T.Text
name' = do
  c <- upperChar
  cs <- many alphaNumChar
  return . T.pack $ c : cs

banner :: String -> IO ()
banner s = putStrLn $ "\n" ++ s' ++ s ++ s'
  where
    n = 80 - length s
    s' = replicate (div n 2) '='
