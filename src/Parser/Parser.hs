module Parser.Parser (parse, getPragmas, getExtensions) where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import "ghc-boot-th" GHC.LanguageExtensions.Type
import "ghc" GHC
import GHC.Paths
import "regex" Text.RE.TDFA.Text hiding ((=~))
import Util.Types
import "regex-tdfa" Text.Regex.TDFA

-- TODO: collect and return all pragmas
parse :: FilePath -> IO RState
parse fileName = do
  modName <- T.init . T.drop (T.length "module ") . getModName <$> TIO.readFile fileName

  (p,t) <- runGhc (Just libdir) $ do
    dflags          <- getSessionDynFlags
    (dflags', _, _) <- parseDynamicFlags dflags []
    _ <- setSessionDynFlags dflags'

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

getModName :: T.Text -> T.Text
getModName = (=~ ("module [A-Z][A-Za-z0-9\\.]+ " :: T.Text))
