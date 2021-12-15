  ./../defaults.dhall
⫽ { name = "build-plugins-for-blobs"
  , synopsis = "A shake build of uom-plugin."
  , description = "Builds the packages making up uom-plugin."
  , category = "Build"
  , executables.build-plugins-for-blobs
    =
    { dependencies =
      [ "base"
      , "ansi-terminal"
      , "dhall"
      , "shake"
      , "raw-strings-qq"
      , "text"
      , "time"
      ]
    , ghc-options = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
    , main = "Main.hs"
    , source-dirs = "."
    }
  }
