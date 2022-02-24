#!/bin/sh

if [ "$(command -v fourmolu)" = "" ] ; then
    stack --silent --allow-different-user install fourmolu-0.3.0.0
fi

find * -name '*.hs' | xargs -P0 fourmolu -i
