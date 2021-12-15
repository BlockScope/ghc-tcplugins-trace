  ./../defaults.dhall
⫽ { name = "build-ghc-tcplugins-trace"
  , synopsis = "A shake build of ghc-tcplugins-trace."
  , description = "Builds the packages making up ghc-tcplugins-trace."
  , category = "Build"
  , executables.build-ghc-tcplugins-trace
    =
    { dependencies =
      [ "base", "ansi-terminal", "shake", "raw-strings-qq", "text", "time" ]
    , ghc-options = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
    , main = "Main.hs"
    , source-dirs = "."
    }
  }
