#!/bin/sh

set -e

for sample_dir in samples/*; do
  # If anything in the samples directory is not directory then don't try
  # to run it as a sample
  if [ -d "$sample_dir" ]; then
    ./scripts/test-sample.sh $sample_dir
  fi
done
