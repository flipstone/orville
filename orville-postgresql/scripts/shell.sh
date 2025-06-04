#!/bin/sh

export STACK_YAML_FILE=$1

set -e

docker compose run --rm dev bash
