FROM ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.3-build-2026.8.7.56@sha256:ba140d293971c86a850b2d20e1636c72554fc7163b4aea7c26c5d0be87c64b96

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
