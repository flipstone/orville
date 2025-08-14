FROM ghcr.io/flipstone/haskell-tools:debian-stable-ghc-9.6.6-2025-08-12-593a4ee

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
