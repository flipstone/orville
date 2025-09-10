if [ "$IN_DEV_CONTAINER" ]; then
  # Already in container, nothing to do
  :
else
  if ! command -v docker compose 2>/dev/null 1>/dev/null; then
    printf "docker compose is used to build, but wasn't found!";
    exit 1;
  else
    docker compose build dev
    exec docker compose run --rm dev $0 "$@"
  fi
fi
