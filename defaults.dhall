{ version = "0.3.0.1"
, author = "Adam Gundry <adam@well-typed.com>"
, maintainer = "Adam Gundry <adam@well-typed.com>"
, copyright = "Copyright (c) 2014-2018, Adam Gundry"
, tested-with =
    "GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.3"
, extra-source-files = "package.dhall"
, dependencies = [ "base >=4.9.1.0 && <5" ]
, when =
  { condition = "impl(ghc >= 8.8.0)"
  , `then`.ghc-options
    =
    [ "-Wall"
    , "-fno-warn-unticked-promoted-constructors"
    , "-fwrite-ide-info"
    , "-hiedir=../.hie"
    ]
  , `else`.ghc-options = [ "-Wall", "-fno-warn-unticked-promoted-constructors" ]
  }
}
