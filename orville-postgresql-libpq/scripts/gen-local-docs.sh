#!/bin/sh

set -e

docker-compose run \
  --no-deps --rm dev \
  stack --stack-yaml stack-lts-17.0.yml \
  haddock --force-dirty --no-haddock-deps --haddock-arguments --odir=local-docs
