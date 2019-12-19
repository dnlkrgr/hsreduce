{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           HsAllInOne                     ( hsAllInOne )
import           System.Environment             ( getArgs )
import           Control.Monad

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
        (test : file : _) -> loop test file
        _ ->
            print
                $  "Usage: "
                ++ "hsreduce "
                ++ "<location-of-test-file> "
                ++ "<location-of-hs-source-file>"

-- | loop runs the fixpoint calculation of the given source code with the given test file
loop :: FilePath -> FilePath -> IO ()
loop test file = do
    fileContent <- readFile file
    case parseStuff fileContent of
        PFailed _ location errMsg -> print "failed to parse file"
        POk pstate pmod           -> do
            -- TODO: write new file content to file
            let newFileContent = showSDoc baseDynFlags . ppr $ unLoc pmod 
                myModule       = unLoc pmod
                myImports      = hsmodImports myModule
                myDecls        = hsmodDecls myModule
            putStrLn newFileContent
            return ()
    return ()
    -- when False $ loop test file

-- | parseStuff takes a string that was read from a haskell 
-- source file and returns a parsed Haskell AST
parseStuff :: String -> ParseResult ParsedSource
parseStuff input = runParser baseDynFlags input Parser.parseModule

runParser :: DynFlags -> String -> P a -> ParseResult a
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
