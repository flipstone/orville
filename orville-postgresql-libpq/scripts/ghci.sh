#!/bin/sh

docker-compose run --rm dev \
  stack --stack-yaml stack-lts-17.3.yml \
  ghci \
  orville-postgresql-libpq:lib \
  orville-postgresql-libpq:test:spec
