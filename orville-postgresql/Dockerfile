FROM ghcr.io/flipstone/haskell-tools:debian-stable-ghc-9.6.6-2024-10-23-eb5da39

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
