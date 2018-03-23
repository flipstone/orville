FROM flipstone/stack:v1-1.6.5

RUN apt-get update &&\
    apt-get install -y --no-install-recommends libpq5 libpq-dev &&\
    apt-get clean

