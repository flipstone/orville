#!/usr/bin/env sh

stack install --resolver lts-11.2 hindent

echo 'IDE Services Running'
exec cat
