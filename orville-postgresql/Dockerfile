FROM ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.3-build-2026.9.10.59@sha256:693944c1fb25917484680ca1f7f4c8c8bea6ef25a9232f8119f773fe14235303

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update &&\
    apt-get install -qq -y --no-install-recommends libpq5 libpq-dev happy &&\
    apt-get clean
