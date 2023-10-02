#!/bin/sh

set -e

docker-compose run \
  --no-deps --rm dev \
  stack --stack-yaml stack-lts-18.28-ghc-8.10.7.yml \
  haddock --force-dirty --no-haddock-deps --haddock-arguments --odir=local-docs
