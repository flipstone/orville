#!/bin/sh

./scripts/stack.sh install
./scripts/test-all-samples.sh
./scripts/site.sh rebuild
