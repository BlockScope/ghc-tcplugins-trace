{ version = "0.1.0"
, author = "Phil de Joux"
, maintainer = "phil.dejoux@blockscope.com"
, copyright = "© 2021-2022 Phil de Joux, © 2021-2022 Block Scope Limited"
, git = "https://github.com/BlockScope/ghc-tcplugins-trace.git"
, bug-reports = "https://github.com/blockscope/ghc-tcplugins-trace/issues"
, license = "MPL-2.0"
, license-file = "LICENSE.md"
, tested-with =
    "GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.4, GHC == 8.10.7, GHC == 9.0.1"
, extra-source-files = "package.dhall"
, ghc-options =
  [ "-Wall"
  , "-Werror"
  , "-Wincomplete-uni-patterns"
  , "-Wcompat"
  , "-Widentities"
  , "-Wredundant-constraints"
  , "-fhide-source-paths"
  ]
}
