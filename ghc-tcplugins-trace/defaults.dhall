{ version = "0.1.0.0"
, author = "Divesh Otwani"
, maintainer = "Ben Gamari"
, license = "MIT"
, license-file = "LICENSE"
, tested-with = "GHC == 8.4.4"
, ghc-options =
  [ "-Wall"
  , "-Werror"
  , "-Wincomplete-uni-patterns"
  , "-Wcompat"
  , "-Widentities"
  , "-Wredundant-constraints"
  , "-fhide-source-paths"
  ]
, dependencies = [ "base" ]
, flags.suppress-failing-tests = { manual = False, default = True }
}
