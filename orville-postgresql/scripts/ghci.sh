#!/bin/sh

docker compose run --rm dev \
  stack --stack-yaml stack.yaml \
  ghci \
  orville-postgresql:lib \
  orville-postgresql:test:spec
