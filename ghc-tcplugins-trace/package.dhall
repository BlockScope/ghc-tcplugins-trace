let defs = ./defaults.dhall

in    defs
    ⫽ { name = "ghc-tcplugins-trace"
      , synopsis = "Tracing of typechecking constraints"
      , description =
          "Tracing pretty printed constraints encountered when typechecking."
      , category = "Development"
      , github = "blockscope/ghc-tcplugins-trace"
      , ghc-options = [ "-Wall", "-fno-warn-partial-type-signatures" ]
      , dependencies =
            defs.dependencies
          # [ "containers"
            , "ghc-corroborate"
            , "template-haskell >=2.9"
            , "hashable"
            , "th-printf"
            ]
      , library =
        { source-dirs = "src"
        , exposed-modules = [ "Plugins.Print.Constraints", "Plugins.Print" ]
        , other-modules = [] : List Text
        , other-extensions = [] : List Text
        }
      }
