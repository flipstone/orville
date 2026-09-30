FROM ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.3-build-2026.9.29.65@sha256:e9f9ce9e125ede479e13595ac8113cde5a3266066404ecc94aad8f4c28c0e3a4

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
