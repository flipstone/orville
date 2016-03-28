FROM haskell:7.10.3

RUN apt-get update &&\
    apt-get install -y --no-install-recommends libpq5 libpq-dev &&\
    apt-get clean

