module Parser.Parser (getPragmas, parse)  where

import qualified Data.List as L
import Data.Maybe
import Data.Void
import Control.Monad.IO.Class
import DynFlags hiding (extensions)
import Path
import Data.Either
import Data.Functor
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import GHC hiding (extensions)
import GHC.Paths
import Util.Types

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
    (    (string "language" <|> string "LANGUAGE") >> return Language)
    <|> ((string "OPTIONS_GHC" <|> string "options_ghc") >> return OptionsGhc)
    <|> ((string "INCLUDE" <|> string "include") >> return Include)


-- TODO: collect and return all pragmas
parse :: Bool -> [Path Abs Dir] -> [Path Abs Dir] -> Path Abs File -> IO RState
parse justParse includeDirs srcDirs fileName = do
    banner $ "Parsing: " ++ fromAbsFile fileName
  
    modName <-
      (\case
        Left _ -> "Main"
        Right t -> t)
      . getModName
      <$> TIO.readFile (fromAbsFile fileName)
  
    putStrLn $ "modName: " <> show modName
  
    extensions <- catMaybes . map pragma2Extension <$> getPragmas fileName
    print extensions
  
  
    (p, et) <- runGhc (Just libdir) $ do
      dflags <- flip (L.foldl' xopt_set) extensions . (\(a,_,_) -> a) <$> (flip parseDynamicFlags [] =<< getSessionDynFlags)
      let 
          includeDirStrings = map fromAbsDir includeDirs
          srcDirStrings     = map fromAbsDir srcDirs
      _ <- setSessionDynFlags dflags { includePaths = IncludeSpecs [] includeDirStrings, importPaths = srcDirStrings }
  
      target <- guessTarget (fromAbsFile fileName) Nothing
      setTargets [target]
      _ <- load LoadAllTargets
      modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName
  
      p <- parseModule modSum
      -- TODO: catch and handle exceptions that could be thrown by typechecking
      t <- if justParse then return $ Left () else Right <$> typecheckModule p
      return (p, t)
  
    prags <- getPragmas fileName
  
    case et of
      Left _  -> return $ RState prags (parsedSource p) Nothing Nothing False
      Right t -> return $ RState prags (parsedSource p) (renamedSource t) (Just $ typecheckedSource t) False

-- -- TODO: collect and return all pragmas
-- parse :: Bool -> [Path Abs Dir] -> [Path Abs Dir] -> Path Abs File -> IO RState
-- parse justParse includeDirs srcDirs fileName = do
--   banner ("Parsing: " <> fromAbsFile fileName)
-- 
--   modName <-
--     (\case
--       Left _ -> "Main"
--       Right t -> t)
--     . getModName
--     <$> TIO.readFile (fromAbsFile fileName)
-- 
--   print $ "Parser.Parser.parse: modName - " <> modName
-- 
--   prags <- getPragmas fileName
--   let extensions = map fromJust . filter isJust . map pragma2Extension $ prags
-- 
--   (p, et) <- runGhc (Just libdir) $ do
--     dflags          <- getSessionDynFlags
--     (dflags1, _, _) <- parseDynamicFlags dflags []
--     let includeDirStrings = map fromAbsDir includeDirs
--         srcDirStrings     = map fromAbsDir srcDirs
--     let dflags2 = L.foldl' xopt_set dflags1 extensions
--     void $ setSessionDynFlags dflags2 { includePaths = IncludeSpecs [] includeDirStrings, importPaths = srcDirStrings }
-- 
-- 
--     target <- guessTarget (fromAbsFile fileName) Nothing
--     setTargets [target]
-- 
--     load LoadAllTargets >>= \case
--       Succeeded -> do
-- 
--         modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName
-- 
--         p <- parseModule modSum
--         -- TODO: catch and handle exceptions that could be thrown by typechecking
--         t <- if justParse then return $ Left () else Right <$> typecheckModule p
-- 
-- 
--         return (p, t)
-- 
--       _ -> error "Parser.Parser.parse: loading of all targets didn't succeed"
-- 
-- 
--   case et of
--     Left _  -> return $ RState prags (parsedSource p) Nothing Nothing False
--     Right t -> return $ RState prags (parsedSource p) (renamedSource t) (Just $ typecheckedSource t) False


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
name = T.cons <$> char '.' <*> name' <|> name'

name' :: Parser T.Text
name' = do
    c <- upperChar
    cs <- many alphaNumChar
    return . T.pack $ c : cs

banner :: MonadIO m => String -> m ()
banner s = liftIO $ putStrLn $ "\n" ++ s' ++ s ++ s'
  where
      n = 80 - length s
      s' = replicate (div n 2) '='
