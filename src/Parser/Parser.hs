module Parser.Parser (getPragmas, parse) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Either
import Data.Functor
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import DynFlags hiding (extensions)
import GHC hiding (extensions)
import GHC.Paths
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

parse :: Bool -> Path Abs File -> IO RState
parse justParse fileName = do
    banner $ "Parsing: " <> fromAbsFile fileName

    let filePath = fromAbsFile fileName
    p <- EP.parseModule filePath >>= \case
        Left e -> error . showSDocUnsafe $ ppr e
        Right p -> return $ snd p

    (mt, hEnv) <-
        if justParse
            then return (Nothing, Nothing)
            else do
                modName <-
                    ( \case
                          Left _ -> "Main"
                          Right t -> t
                        )
                        . getModName
                        <$> TIO.readFile filePath

                putStrLn $ "modName: " <> show modName

                extensions <- catMaybes . map pragma2Extension <$> getPragmas fileName

                runGhc (Just libdir) $ do
                    dflags <- flip (L.foldl' xopt_set) extensions . (\(a, _, _) -> a) <$> (flip parseDynamicFlags [] =<< getSessionDynFlags)
                    _ <- setSessionDynFlags dflags

                    target <- guessTarget (fromAbsFile fileName) Nothing
                    setTargets [target]
                    _ <- load LoadAllTargets
                    modSum <- getModSummary . mkModuleName . T.unpack $ if modName == "" then "Main" else modName

                    pm <- parseModule modSum
                    -- TODO: catch and handle exceptions that could be thrown by typechecking
                    t <- typecheckModule pm

                    hEnv <- getSession

                    return (Just t, Just hEnv)

    prags <- getPragmas fileName

    return $ RState prags p mt False emptyStats 0 0 hEnv

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