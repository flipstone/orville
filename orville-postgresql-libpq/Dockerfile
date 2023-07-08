FROM ghcr.io/flipstone/haskell-tools:debian-unstable-ghc-9.2.7-2023-06-29-27bed39
RUN apt-get update &&\
    apt-get install -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
