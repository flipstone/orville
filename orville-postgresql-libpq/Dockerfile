FROm ghcr.io/flipstone/haskell-tools:debian-unstable-ghc-9.4.5-2023-07-18-1d8bf61

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
