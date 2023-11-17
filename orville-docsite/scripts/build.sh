#!/bin/sh

set -e

./scripts/stack.sh install
./scripts/test-all-samples.sh
./scripts/site.sh rebuild
rm -r ../docs/*
cp -r site-builder/_site/* ../docs/.
