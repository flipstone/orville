#!/bin/sh

set -e

docker compose run \
  --no-deps --rm dev \
  stack --stack-yaml stack.yml \
  haddock --force-dirty --no-haddock-deps --haddock-arguments --odir=local-docs
