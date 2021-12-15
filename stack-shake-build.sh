#!/bin/sh

#set +v

stack install build-ghc-tcplugins-trace
build-ghc-tcplugins-trace $@
