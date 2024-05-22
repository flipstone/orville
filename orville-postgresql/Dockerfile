FROM ghcr.io/flipstone/haskell-tools:debian-stable-ghc-9.6.5-2024-05-15-5314c42

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
