let Prelude =
      https://prelude.dhall-lang.org/v19.0.0/package.dhall

let prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.3.4.0/dhall/prelude.dhall

let types =
      https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.3.4.0/dhall/types.dhall



let defaultExtensions = 
      [ types.Extension.DataKinds                   True
      , types.Extension.DeriveGeneric               True
      , types.Extension.FlexibleContexts            True
      , types.Extension.FlexibleInstances           True
      , types.Extension.GeneralizedNewtypeDeriving  True
      , types.Extension.LambdaCase                  True
      , types.Extension.MultiParamTypeClasses       True
      , types.Extension.OverloadedStrings           True
      , types.Extension.PatternSynonyms             True
      , types.Extension.QuasiQuotes                 True
      , types.Extension.Rank2Types                  True
      , types.Extension.RecordWildCards             True
      , types.Extension.ScopedTypeVariables         True
      , types.Extension.TemplateHaskell             True
      , types.Extension.TupleSections               True
      , types.Extension.TypeFamilies                True
      , types.Extension.TypeOperators               True
      , types.Extension.UndecidableInstances        True
      , types.Extension.ViewPatterns                True
      , types.Extension.TypeApplications            True
      ]

let commonStuff = 
      { default-extensions  = defaultExtensions
      , default-language    = Some types.Language.Haskell2010
      }

let makeBuildDepend = \(name : Text) -> { bounds = prelude.anyVersion, package = name }
let buildDepends = 
      Prelude.List.map Text types.Dependency makeBuildDepend
      [ "base" 
      , "HUnit" 
      , "Cabal" 
      , "MonadRandom" 
      , "aeson" 
      , "bytestring" 
      , "cassava" 
      , "unordered-containers" 
      , "containers" 
      , "edit-distance" 
      , "ghc" 
      , "ghc-boot-th" 
      , "ghc-exactprint" 
      , "ghc-paths" 
      , "hashable" 
      , "hie-bios" 
      , "katip" 
      , "lifted-async" 
      , "Diff" 
      , "lifted-base" 
      , "megaparsec" 
      , "microlens-platform" 
      , "monad-control" 
      , "mtl" 
      , "optparse-generic" 
      , "path" 
      , "path-io" 
      , "process" 
      , "regex" 
      , "split" 
      , "stm-lifted" 
      , "syb" 
      , "text" 
      , "time" 
      , "uniplate" 
      , "word8"
      , "transformers-base"
      ]

let prependReducePassesPrefix = \(prefix : Text) -> "Reduce.Passes." ++ prefix
let passes = 
              Prelude.List.map Text Text prependReducePassesPrefix
                [ "Expr"
                , "Types"
                , "Pat"
                , "Decls"
                , "TypeFamilies"
                , "Stubbing"
                , "DataTypes"
                , "Typeclasses"
                , "Names"
                , "Functions"
                , "Imports"
                , "Exports"
                , "Pragmas"
                , "Parameters"
                , "TemplateHaskell"
                ]

in prelude.defaults.Package
    // { name          = "hsreduce"
       , version       = prelude.v "0.1.0.0"
       , build-type    = Some types.BuildType.Simple
       , cabal-version = prelude.v "2.4"
       , executables   =
           [ { executable =
                   \(config : types.Config)
                 ->   prelude.defaults.Executable
                   // commonStuff
                   // { main-is = "Main.hs"
                     , build-depends = buildDepends # [{ bounds = prelude.anyVersion , package = "lib-hsreduce" }]
                     , compiler-options =
                           prelude.defaults.CompilerOptions
                         // { GHC =
                               [ "-Wall"
                               , "-threaded"
                               , "-rtsopts"
                               , "-with-rtsopts=-T"
                               , "-Wno-missing-methods"
                               , "-Wno-orphans"
                               ] : List Text
                            }
                     , hs-source-dirs      = [ "src" ]
                     }
             , name = "hsreduce"
             }
           ]
       , sub-libraries =
           [ { library =
                   \(config : types.Config)
                 ->   
                   prelude.defaults.Library
                     // commonStuff
                     // { build-depends       = buildDepends
                        , compiler-options    = prelude.defaults.CompilerOptions // { GHC = [ "-Wall" ] : List Text }
                        , hs-source-dirs      = [ "src" ]
                        , other-modules       = [ "Parser.Parser" ]
                        , exposed-modules =
                            passes #
                            [ "Merge.HsAllInOne"
                            , "Reduce.Driver"
                            , "Reduce.Passes"
                            , "Util.Util"
                            , "Util.Types"
                            ]

                        }
             , name = "lib-hsreduce"
             }
           ]
       , test-suites =
           [ { name = "test-hsreduce"
             , test-suite =
                 \(config : types.Config)
                   -> prelude.defaults.TestSuite
                      // commonStuff
                      // { type                = types.TestType.exitcode-stdio { main-is = "Test.hs" }
                         , build-depends       = 
                              buildDepends 
                            # Prelude.List.map Text types.Dependency makeBuildDepend [ "lib-hsreduce", "hspec" ]
                         , compiler-options    = prelude.defaults.CompilerOptions // { GHC = [ "-Wall" ] : List Text }
                         , hs-source-dirs      = [ "src", "test" ]
                         }
             }
           ]
       , license =
           types.License.SPDX
           ( prelude.SPDX.license
             types.LicenseId.BSD_3_Clause
             (None types.LicenseExceptionId)
           )
       , extra-source-files = [ "CHANGELOG.md" ]
       , license-files = [ "LICENSE" ]
       }