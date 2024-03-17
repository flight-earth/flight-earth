#!/bin/sh

#set +v

cabal install fe-build --overwrite-policy=always --installdir=$HOME/.cabal/bin
fe-build $@
