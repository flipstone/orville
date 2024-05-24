#!/bin/sh

docker compose run --rm dev \
  stack --stack-yaml stack.yml \
  ghci \
  orville-postgresql:lib \
  orville-postgresql:test:spec
