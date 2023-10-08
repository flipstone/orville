#!/bin/sh

# dev is specified here so that docker-compose up will only attached to the dev
# container and not the db container. A number of the tests produce errors logs
# in the database container which are expected for negative testing (e.g.
# primary key violations, not null violations). These logs end up interspersed
# throughout the test results, which is annoying. If you need to see the logs
# for debugging, you can run `docker-compose logs testdb`, or just run
# `docker-compose up` directly.
docker-compose up dev
