#!/bin/sh

ghcup install ghc latest --set

cabal update
cabal test --flag ci
