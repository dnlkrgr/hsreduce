{-# language OverloadedStrings #-}
module Main
    ( main
    )
where

import           HsAllInOne                     ( hsAllInOne )
import           System.Environment             ( getArgs )
import           Control.Monad
import           System.Process
import           System.Exit
import           System.IO
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

-- GHC API Stuff
import           GHC
import           Lexer
import           Parser
import           SrcLoc
import           FastString
import           StringBuffer
import           DynFlags
import           Platform
import           Config
import           Fingerprint
import           HsSyn
import           HsExtension
import           Outputable

main :: IO ()
main = do
    args <- getArgs
    case args of
        (test : file : _) -> do
            fileContent <- TIO.readFile file
            loop test file (T.unpack fileContent)
            return ()
        _ ->
            print
                $  "Usage: "
                ++ "hsreduce "
                ++ "<location-of-test-file> "
                ++ "<location-of-hs-source-file>"

-- | loop runs the fixpoint calculation of the given source code with the given test file
loop
    :: FilePath -- ^ location of test file to check whether reduced file is interesting
    -> FilePath -- ^ location of source file
    -> String
    -> IO ()
loop test file oldFileContent = do
    case parseStuff oldFileContent of
        PFailed _ location errMsg -> print "failed to parse file"
        POk pstate pmod           -> do
            -- TODO: write new file content to file
            let newFileContent = showSDoc baseDynFlags . ppr $ unLoc pmod
                myModule       = unLoc pmod
                myImports      = hsmodImports myModule
                myDecls        = hsmodDecls myModule
            -- reduce imports
            -- reduce decls
            -- write to source file
            TIO.writeFile file (T.pack newFileContent)
            -- call test script
            (exitCode, _, _) <- readProcessWithExitCode test [] "stdin"
            case exitCode of
                ExitFailure _ -> do
                    putStrLn "test exited with failure"
                    putStrLn oldFileContent
                    return ()
                ExitSuccess -> do
                    putStrLn "test exited with success"
                    putStrLn newFileContent
                    --loop test file newFileContent
                    return ()
            return ()
    return ()

--reduceStep :: FilePath -> FileContent -> FilePath -> [Int] -> IO ()
--reduceStep test oldFileContent filePath triedLines = do
--    (i, newFileContent) <- naivePass oldFileContent triedLines
--    TIO.writeFile filePath newFileContent
--    (exitCode, _, _) <- readProcessWithExitCode test [] "stdin"
--    case exitCode of
--        ExitSuccess -> length $ T.lines newFileContent $ reduceStep
--            test
--            newFileContent
--            filePath
--            []
--        ExitFailure _ ->
--            if length (i : triedLines) < length (T.lines newFileContent)
--                then reduceStep test oldFileContent filePath (i : triedLines)
--                else TIO.writeFile filePath oldFileContent


-- | parseStuff takes a string that was read from a haskell 
-- source file and returns a parsed Haskell AST
parseStuff
    :: String                       -- ^ file content of source file
    -> ParseResult ParsedSource     -- ^ parsed AST
parseStuff input = runParser baseDynFlags input Parser.parseModule

runParser
    :: DynFlags
    -> String           -- ^ file content of source file
    -> P a              -- ^ parser
    -> ParseResult a    -- ^ parsed AST
runParser flags str parser = unP parser parseState
  where
    filename   = "<interactive>"
    location   = mkRealSrcLoc (mkFastString filename) 1 1
    buffer     = stringToStringBuffer str
    parseState = mkPState flags buffer location

-- dummy data needed by the GHC parser
baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

fakeSettings :: Settings
fakeSettings = Settings { sTargetPlatform    = platform
                        , sPlatformConstants = platformConstants
                        , sProjectVersion    = cProjectVersion
                        , sProgramName       = "ghc"
                        , sOpt_P_fingerprint = fingerprint0
                        }
  where
    platform = Platform { platformWordSize       = 8
                        , platformOS             = OSUnknown
                        , platformUnregisterised = True
                        }
    platformConstants = PlatformConstants { pc_DYNAMIC_BY_DEFAULT = False
                                          , pc_WORD_SIZE          = 8
                                          }

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
