#!/bin/sh

set -e

sample_dir=$1

if [ "$1" = "" ]; then
  echo "path to sample directory must be specified"
  exit 1
fi;

echo "===================================================="
echo "Running $sample_dir"
echo "============================i======================="

docker compose run \
  --rm \
  --workdir /orville-docsite/$sample_dir \
  dev \
  sh ./run.sh
