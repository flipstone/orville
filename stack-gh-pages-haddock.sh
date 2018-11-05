#!/bin/sh

# Based on https://github.com/yamadapc/stack-gh-pages
set -e

stack haddock --haddock-hyperlink-source --no-haddock-deps

version_number=`cat orville.cabal | grep "version:" | head -n 1 | awk '{print $2}'`
docs=`stack path --local-doc-root`/orville-$version_number

git stash
git branch -D gh-pages
git checkout --orphan gh-pages

rm -rf *
cp -r $docs/* .
git add .
git commit -m "Automated Haddock commit"
git push -f -u origin gh-pages
git checkout master
