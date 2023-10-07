#!/bin/sh

if [ "$(command -v fourmolu)" = "" ] ; then
    echo "Fourmolu not found, installing 0.13.1.0, with nightly-2023-07-26"
    stack --resolver nightly-2023-07-26 --silent --allow-different-user install fourmolu-0.13.1.0
fi

find * -name '*.hs' | xargs -P0 fourmolu -i
