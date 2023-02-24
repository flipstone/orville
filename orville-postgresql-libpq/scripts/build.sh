#!/bin/sh

set -e

SHOULD_BE_VERBOSE=0
SHOULD_SKIP_IMAGE_BUILD=0

# Process arguments at the beginning to influence verbosity as early as possible.
for arg in "$@"
do
  case ${arg} in
    -v|--verbose)
      SHOULD_BE_VERBOSE=1
      shift
      ;;
    --skip-image-build)
      SHOULD_SKIP_IMAGE_BUILD=1
      shift
      ;;
    -h|--help)
      printf "Usage: build.sh [--verbose] [--skip-image-build]"
      exit 0;
      ;;
    *)
      if [ "$#" -gt 0 ]; then
        shift; # shift only if possible
      fi
      ;;
  esac
done


# define helper functions

## echo only on verbose mode
echo_when_verbose() {
  if [ "$SHOULD_BE_VERBOSE" -eq 1 ]; then
    echo "$1"
  fi
}

if ! command -v docker-compose 2>/dev/null 1>/dev/null; then
  printf "docker-compose is used to build, but wasn't found!";
  exit 1;
else
  # the actual logic of what "build" means

  if [ "$SHOULD_SKIP_IMAGE_BUILD" -eq 0 ]; then
    echo_when_verbose "We start by ensuring the docker image is up to date\n"
    docker-compose build
  fi

  echo_when_verbose "Going to run formatting against the codebase.\n"
  docker-compose run --rm dev sh ./scripts/format-repo.sh

  echo_when_verbose "Now verifying documentation.\n"
  ( cd ../docs
    docker-compose run --rm --build docs /orville-root/GETTING-STARTED.md
    docker-compose run --rm --build docs /orville-root/SQL-MARSHALLER.md
    docker-compose run --rm --build docs /orville-root/PLAN.md
    docker-compose run --rm --build docs /orville-root/JSON.md
  )

  echo_when_verbose "Now running the tests against the supported stack resolvers.\n"
  docker-compose run --rm dev sh ./scripts/test-all

fi
