module Parser.Parser (parse, getPragmas) where

import Data.Either
import Data.Functor
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import "ghc-boot-th" GHC.LanguageExtensions.Type
import "ghc" GHC
import "ghc" DynFlags
import GHC.Paths
import "regex" Text.RE.TDFA.Text hiding ((=~))
import Util.Types
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import Data.Void


type Parser = M.Parsec Void T.Text



-- TODO: collect and return all pragmas
parse :: FilePath -> [FilePath] -> IO RState
parse fileName includeDirs = do
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
    _ <- setSessionDynFlags dflags' { includePaths = IncludeSpecs [] includeDirs }

    target <- guessTarget fileName Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName

    p <- parseModule modSum 
    -- TODO: catch exceptions that could be thrown by typechecking?
    t <- typecheckModule p
    return (p, t)

  prags <- map Language . getExtensions <$> TIO.readFile fileName
  return $ RState prags (parsedSource p) (renamedSource t) (Just $ typecheckedSource t)


-- TODO: use regexes here
getExtensions :: T.Text -> [Extension]
getExtensions source = 
  filter ((`T.isInfixOf` source) . T.pack . show)
         $ allExtensions

allExtensions :: [Extension]
allExtensions =  map toEnum [0..114]

-- TODO: this doesn't match all pragmas
getPragmas :: T.Text -> [T.Text]
getPragmas = fromMaybe [] . traverse matchedText . allMatches . (*=~ [re|{-# +([A-Z][A-Za-z]+) +#-}|])

-- BUG: parse error bei ListUtils, weiss noch nicht warum
getModName :: T.Text -> Either (M.ParseErrorBundle T.Text Void) T.Text
getModName = fmap T.concat . sequence . filter isRight . map (M.parse getModName' "") . T.lines

getModName' :: Parser T.Text
getModName' = do
  void $ space
  void $ string "module"
  void $ space
  n <- T.concat <$> some name
  void $ space
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
