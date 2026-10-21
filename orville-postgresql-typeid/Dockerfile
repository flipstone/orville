FROM ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.3-build-2026.10.21.67@sha256:d1e44470d87c65de63239c10ccde33e6254e8cf9ce4ec54071461c34cb30b450

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
