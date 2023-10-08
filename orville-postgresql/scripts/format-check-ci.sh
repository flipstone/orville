#!/bin/sh

NUM_FILES_CHANGED=$(git status --porcelain | grep -v stack-root | wc -l)
if [ "$NUM_FILES_CHANGED" -gt 0 ]; then
    printf "Formatting resulted in a diff! Please run the formatting in the repo\n"
    git status --porcelain
    exit 1;
fi
