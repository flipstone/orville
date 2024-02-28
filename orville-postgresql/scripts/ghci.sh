#!/bin/sh

docker compose run --rm dev \
  stack --stack-yaml stack-lts-17.3.yml \
  ghci \
  orville-postgresql:lib \
  orville-postgresql:test:spec
