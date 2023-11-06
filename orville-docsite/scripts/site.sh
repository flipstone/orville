#!/bin/sh

set -e

docker compose run \
  --rm \
  --service-ports \
  --workdir /orville-docsite/site-builder \
  dev \
  stack exec site -- "$@"
