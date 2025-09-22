FROM ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.2-8edd338

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
