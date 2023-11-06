#!/bin/sh

set -e

docker compose run --rm dev stack "$@"
