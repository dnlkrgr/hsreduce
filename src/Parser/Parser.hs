module Parser.Parser (parse, getPragmas, mod2DepNames) where

import Path
import Data.Either
import Data.Functor
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC
import DynFlags
import GHC.Paths
import Util.Types
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import Data.Void

import Control.Monad
import System.Directory
import Data.Maybe

type Parser = M.Parsec Void T.Text

mod2DepNames :: [Path Abs Dir]
             -> [Path Abs Dir]
             -> Path Abs File
             -> IO [Path Abs File]
mod2DepNames includeDirs srcDirs fileName = do

  mg <- runGhc (Just libdir) $ do
    (dflags', _, _) <- getSessionDynFlags >>= flip parseDynamicFlags []

    let includeDirStrings = map fromAbsDir includeDirs
        srcDirStrings     = map fromAbsDir srcDirs
    _ <- setSessionDynFlags dflags' { includePaths = IncludeSpecs [] includeDirStrings, importPaths = srcDirStrings }

    target <- guessTarget (fromAbsFile fileName) Nothing
    setTargets [target]
    _ <- load LoadAllTargets

    getModuleGraph

  -- preprocess mod names
  let relModPaths =
         map (fromJust . (addFileExtension "hs" <=< parseRelFile)
                 . map (\c -> if c == '.' then '/' else c)
                 . moduleNameString
                 . ms_mod_name)
       . mgModSummaries
       $ mg

  filterM (doesFileExist  . fromAbsFile) . concatMap (\d -> map (d </>) relModPaths) $ srcDirs

-- TODO: collect and return all pragmas
parse :: Bool -> [Path Abs Dir] -> [Path Abs Dir] -> Path Abs File -> IO RState
parse justParse includeDirs srcDirs fileName = do
  banner ("Parsing: " <> fromAbsFile fileName)

  modName <-
    (\case
      Left _ -> "Main"
      Right t -> t)
    . getModName
    <$> TIO.readFile (fromAbsFile fileName)

  print modName

  (p, et) <- runGhc (Just libdir) $ do
    dflags          <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags []
    let includeDirStrings = map fromAbsDir includeDirs
        srcDirStrings     = map fromAbsDir srcDirs
    _ <- setSessionDynFlags dflags' { verbosity = 3, includePaths = IncludeSpecs [] includeDirStrings, importPaths = srcDirStrings }
    -- _ <- setSessionDynFlags dflags' { verbosity = 1, includePaths = IncludeSpecs [] includeDirStrings, importPaths = "/home/daniel/workspace/hsreduce-test-cases/protocol-buffers" : srcDirStrings }

    target <- guessTarget (fromAbsFile fileName) Nothing
    setTargets [target]

    load LoadAllTargets >>= \case
      Succeeded -> do

        modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName

        p <- parseModule modSum
        -- TODO: catch and handle exceptions that could be thrown by typechecking
        t <- if justParse then return $ Left () else Right <$> typecheckModule p


        return (p, t)
      _ -> error "load didn't succeed"

  prags <- getPragmas fileName

  case et of
    Left _ -> return $ RState prags (parsedSource p) Nothing Nothing False
    Right t -> return $ RState prags (parsedSource p) (renamedSource t) (Just $ typecheckedSource t) False


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
name =
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
