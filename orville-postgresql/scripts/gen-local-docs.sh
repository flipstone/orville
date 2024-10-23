#!/bin/sh

set -e

docker compose run \
  --no-deps --rm dev \
  stack --stack-yaml stack.yaml \
  haddock --force-dirty --no-haddock-deps --haddock-arguments --odir=local-docs
