module ReduceParser (parseStuff, baseDynFlags) where

-- TODO: be able to parse signatures with forall

import Config
import DynFlags
import FastString
import Fingerprint
import GHC
import HsAllInOne (hsAllInOne)
import HsExtension
import HsSyn
import Lexer
import Outputable
import Parser
import Platform
import SrcLoc
import StringBuffer

-- | parseModule takes a string that was read from a haskell
-- source file and returns a parsed Haskell AST
parseStuff ::
  -- | file content of haskell file
  String ->
  -- | file path of the haskell file
  FilePath ->
  -- | parsed AST
  ParseResult ParsedSource
parseStuff path input = runParser baseDynFlags path input Parser.parseModule

runParser ::
  DynFlags ->
  -- | file path of the haskell file
  FilePath ->
  -- | file content of haskell file
  String ->
  -- | parser
  P a ->
  -- | parsed AST
  ParseResult a
runParser flags path str parser = unP parser parseState
  where
    filename = path
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

-- dummy data needed by the GHC parser
baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

fakeSettings :: Settings
fakeSettings = Settings
  { sTargetPlatform = platform,
    sPlatformConstants = platformConstants,
    sProjectVersion = cProjectVersion,
    sProgramName = "ghc",
    sOpt_P_fingerprint = fingerprint0
  }
  where
    platform = Platform
      { platformWordSize = 8,
        platformOS = OSUnknown,
        platformUnregisterised = True
      }
    platformConstants = PlatformConstants
      { pc_DYNAMIC_BY_DEFAULT = False,
        pc_WORD_SIZE = 8
      }

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
