FROM ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.3-build-2026.9.11.62@sha256:bef7312b41630fb7dcfd9e39c4db977c0f21078794ded50d9b2935ab1f99e143

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
