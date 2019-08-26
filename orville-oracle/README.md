
# Orville Oracle

[![Build Status](https://secure.travis-ci.org/flipstone/orville.svg)](http://travis-ci.org/flipstone/orville)

A port of Orville to work with Oracle database.
This is very much a work in progress and many features either do not work or remain untested.

## Features/Functions known to work
   - `selectAll`
   - various forms of where clauses including:
     - `whereAnd`
     - `where_`
     - `whereOr`
   - `updateSql`
   - `insertRecord`*
   - certain column types including:
     - `textField`
     - `numberIntField`
     - `numberDoubleField`

   * Note that `insertRecord` had been modified and does not have the same api as `orville-postgresql`

## Features known to not work
   - `selectFirst`

## Differences from `orville-postgresql`
   - `insertRecord` has been modified and does not return the newly inserted data.
   - `textField`
   - `numberIntField` refers to the Oracle NUMBER column type specified to be an integer
   - `numberDoubleField` refers to the Oracle NUMBER column type specified to be a double

## Untested
   Features not listed above may not have been tested and thus may fail in odd and confusing ways