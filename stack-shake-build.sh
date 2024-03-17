#!/bin/sh

#set +v

stack install fe-build
fe-build $@
