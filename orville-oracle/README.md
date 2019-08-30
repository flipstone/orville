
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

## Setting Up Local Development

### Downloading Closed Source Bits
    To get started we need a few closed source libraries from Oracle. Unfortunately these are also
    behind a registration wall to download. To get these do the following:
    - Go to this page https://www.oracle.com/database/technologies/instant-client/linux-x86-64-downloads.html
    - download both of the following(instantclient files are gitignored to prevent them from being including in the repo on accident)
      and place them in the orville-oracle directory (alongside this file)
      - instantclient-basic-linux.x64-12.1.0.2.0.zip
      - instantclient-odbc-linux.x64-12.1.0.2.0.zip

### Starting and Setting up Oracle
    This is a two step process that assumes you are inside the directory along with this file:
    - First, run `docker-compose up --build oracle`
    - The above will give you a notice about the database being up (along with other log output)
    - After the database is up run `./setup-orville-user-oracle-docker.sh`

### Running a development repl
    The `dev` service defined in the `docker-compose.yml` has environment variables setup for running a repl.
    Using your prefered method to get a shell in the container you can then do `stack --stack-yaml stack-lts-13.12.yml ghci`
    to get a repl. The environment variable `TEST_CONN_STRING` has values needed to create a connection pool (i.e.
    the final argument to `createConnectionPool`).
