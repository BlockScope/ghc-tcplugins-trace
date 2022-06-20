let defs = ./defaults.dhall

in    defs
    ⫽ { name = "ghc-tcplugins-trace"
      , synopsis = "Tracing of typechecking constraints"
      , description =
          "Tracing pretty printed constraints encountered when typechecking."
      , category = "Development"
      , github = "blockscope/ghc-tcplugins-trace"
      , ghc-options = [ "-Wall", "-fno-warn-partial-type-signatures" ]
      , dependencies = defs.dependencies # [ "ghc-corroborate >= 1.0" ]
      , library =
        { source-dirs = "src"
        , exposed-modules = [ "Plugins.Print" ]
        , other-modules = [ "Plugins.Print.Flags", "Plugins.Print.Constraints" ]
        , other-extensions = [] : List Text
        }
      }
