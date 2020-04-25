module Parser.Parser (parse, getPragmas, mod2DepNames) where

import Path
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

mod2DepNames :: [Path Abs Dir]
             -> [Path Abs Dir]
             -> Path Abs File
             -> IO [Path Abs File]
mod2DepNames includeDirs srcDirs fileName = do

  mg <- runGhc (Just libdir) $ do
    dflags          <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags []

    let includeDirStrings = map fromAbsDir includeDirs
        srcDirStrings     = map fromAbsDir srcDirs
    _ <-
      setSessionDynFlags dflags' { includePaths = IncludeSpecs [] includeDirStrings, importPaths = srcDirStrings }

    target <- guessTarget (fromAbsFile fileName) Nothing
    setTargets [target]
    _ <- load LoadAllTargets

    getModuleGraph

  let rootPath = parent fileName


  traverse (\f -> parseRelFile f
             >>= addFileExtension "hs"
             >>= return . (rootPath </>))
    . map (map (\c -> if c == '.' then '/' else c)
           . moduleNameString
           . ms_mod_name)
    . mgModSummaries
    $ mg

-- TODO: collect and return all pragmas
parse :: [Path Abs Dir] -> [Path Abs Dir] -> Path Abs File -> IO RState
parse includeDirs srcDirs fileName = do
  banner $ "Parsing: " ++ (fromAbsFile fileName)

  modName <-
    (\case
      Left _ -> "Main"
      Right t -> t)
    . getModName
    <$> TIO.readFile (fromAbsFile fileName)

  print modName

  (p,t) <- runGhc (Just libdir) $ do
    dflags          <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags []
    let includeDirStrings = map fromAbsDir includeDirs
        srcDirStrings     = map fromAbsDir srcDirs
    _ <- setSessionDynFlags dflags' { includePaths = IncludeSpecs [] includeDirStrings, importPaths = srcDirStrings }

    target <- guessTarget (fromAbsFile fileName) Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName

    p <- parseModule modSum
    -- TODO: catch and handle exceptions that could be thrown by typechecking
    t <- typecheckModule p
    return (p, t)

  prags <- getPragmas fileName

  return $ RState prags (parsedSource p) (renamedSource t) (Just $ typecheckedSource t) False

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
