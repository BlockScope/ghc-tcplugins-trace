#!/bin/sh

#set +v

cabal v2-build build-ghc-tcplugins-trace
exec cabal v2-run build-ghc-tcplugins-trace -- "$@"