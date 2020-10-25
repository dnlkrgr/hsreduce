let prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.3.4.0/dhall/prelude.dhall

let types =
      https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.3.4.0/dhall/types.dhall

let buildDepends = 
      [ { bounds = prelude.anyVersion, package = "base" }
      , { bounds = prelude.anyVersion, package = "HUnit" }
      , { bounds = prelude.anyVersion, package = "Cabal" }
      , { bounds = prelude.anyVersion, package = "MonadRandom" }
      , { bounds = prelude.anyVersion, package = "aeson" }
      , { bounds = prelude.anyVersion, package = "bytestring" } 
      , { bounds = prelude.anyVersion, package = "cassava" }
      , { bounds = prelude.anyVersion, package = "unordered-containers" }
      , { bounds = prelude.anyVersion, package = "containers" }
      , { bounds = prelude.anyVersion, package = "edit-distance" }
      , { bounds = prelude.anyVersion, package = "ghc" }
      , { bounds = prelude.anyVersion, package = "ghc-boot-th" }
      , { bounds = prelude.anyVersion, package = "ghc-exactprint" }
      , { bounds = prelude.anyVersion, package = "ghc-paths" }
      , { bounds = prelude.anyVersion, package = "hashable" }
      , { bounds = prelude.anyVersion, package = "hie-bios" }
      , { bounds = prelude.anyVersion, package = "katip" }
      , { bounds = prelude.anyVersion, package = "lifted-async" }
      , { bounds = prelude.anyVersion, package = "Diff" }
      , { bounds = prelude.anyVersion, package = "lifted-base" }
      , { bounds = prelude.anyVersion, package = "megaparsec" }
      , { bounds = prelude.anyVersion, package = "microlens-platform" }
      , { bounds = prelude.anyVersion, package = "monad-control" }
      , { bounds = prelude.anyVersion, package = "mtl" }
      , { bounds = prelude.anyVersion, package = "optparse-generic" }
      , { bounds = prelude.anyVersion, package = "path" }
      , { bounds = prelude.anyVersion, package = "path-io" }
      , { bounds = prelude.anyVersion, package = "process" }
      , { bounds = prelude.anyVersion, package = "regex" }
      , { bounds = prelude.anyVersion, package = "split" }
      , { bounds = prelude.anyVersion, package = "stm-lifted" }
      , { bounds = prelude.anyVersion, package = "syb" }
      , { bounds = prelude.anyVersion, package = "text" }
      , { bounds = prelude.anyVersion, package = "time" }
      , { bounds = prelude.anyVersion, package = "uniplate" }
      , { bounds = prelude.anyVersion, package = "word8" }
      , { bounds = prelude.anyVersion, package = "transformers-base" }
      ]

let defaultExtensions = 
      [ types.Extension.DataKinds True
      , types.Extension.DeriveGeneric True
      , types.Extension.FlexibleContexts True
      , types.Extension.FlexibleInstances True
      , types.Extension.GeneralizedNewtypeDeriving True
      , types.Extension.LambdaCase True
      , types.Extension.MultiParamTypeClasses True
      , types.Extension.OverloadedStrings True
      , types.Extension.PatternSynonyms True
      , types.Extension.QuasiQuotes True
      , types.Extension.Rank2Types True
      , types.Extension.RecordWildCards True
      , types.Extension.ScopedTypeVariables True
      , types.Extension.TemplateHaskell True
      , types.Extension.TupleSections True
      , types.Extension.TypeFamilies True
      , types.Extension.TypeOperators True
      , types.Extension.UndecidableInstances True
      , types.Extension.ViewPatterns True
      , types.Extension.TypeApplications True
      ]

in    prelude.defaults.Package
    ⫽ { name = "hsreduce"
      , version = prelude.v "0.1.0.0"
      , build-type = Some types.BuildType.Simple
      , cabal-version = prelude.v "2.4"
      , executables =
          [ { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is = "Main.hs"
                    , build-depends =
                          [{ bounds = prelude.anyVersion
                          , package = "lib-hsreduce"
                          }] 
                          # buildDepends
                        
                    , compiler-options =
                          prelude.defaults.CompilerOptions
                        ⫽ { GHC =
                              [ "-Wall"
                              , "-threaded"
                              , "-rtsopts"
                              , "-with-rtsopts=-T"
                              , "-Wno-missing-methods"
                              , "-Wno-orphans"
                              ] : List Text
                          }
                    , default-extensions = defaultExtensions
                    , default-language = Some types.Language.Haskell2010
                    , hs-source-dirs = [ "src" ]
                    }
            , name = "hsreduce"
            }
          ]
      , extra-source-files =
          [ "CHANGELOG.md" ]
      , license =
          types.License.SPDX
          ( prelude.SPDX.license
            types.LicenseId.BSD_3_Clause
            (None types.LicenseExceptionId)
          )
      , license-files =
          [ "LICENSE" ]
      , sub-libraries =
          [ { library =
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends = buildDepends
                    , compiler-options =
                          prelude.defaults.CompilerOptions
                        ⫽ { GHC = [ "-Wall" ] : List Text }
                    , default-extensions = defaultExtensions
                    , default-language = Some types.Language.Haskell2010
                    , hs-source-dirs = [ "src" ]
                    , other-modules = [ "Parser.Parser" ]
                    , exposed-modules =
                        [ "Merge.HsAllInOne"
                        , "Reduce.Driver"
                        , "Reduce.Passes"
                        , "Reduce.Passes.Expr"
                        , "Reduce.Passes.Types"
                        , "Reduce.Passes.Pat"
                        , "Reduce.Passes.Decls"
                        , "Reduce.Passes.TypeFamilies"
                        , "Reduce.Passes.Stubbing"
                        , "Reduce.Passes.DataTypes"
                        , "Reduce.Passes.Typeclasses"
                        , "Reduce.Passes.Names"
                        , "Reduce.Passes.Functions"
                        , "Reduce.Passes.Imports"
                        , "Reduce.Passes.Exports"
                        , "Reduce.Passes.Pragmas"
                        , "Reduce.Passes.Parameters"
                        , "Util.Util"
                        , "Util.Types"
                        ]
                    }
            , name =
                "lib-hsreduce"
            }
          ]
      , test-suites =
          [ { name = "test-hsreduce"
            , test-suite =
                  λ(config : types.Config)
                →   prelude.defaults.TestSuite
                  ⫽ { type =
                        types.TestType.exitcode-stdio { main-is = "Test.hs" }
                    , build-depends =
                        [ { bounds = prelude.anyVersion , package = "lib-hsreduce" }
                        , { bounds = prelude.anyVersion , package = "hspec" }
                        ] 
                        # buildDepends
                    , compiler-options =
                          prelude.defaults.CompilerOptions
                        ⫽ { GHC = [ "-Wall" ] : List Text }
                    , default-extensions = defaultExtensions
                    , default-language = Some types.Language.Haskell2010
                    , hs-source-dirs = [ "src", "test" ]
                    }
            }
          ]
      }
