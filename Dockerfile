FROM flipstone/stack:v2-1.7.1

RUN apt-get update &&\
    apt-get install -y --no-install-recommends libpq5 libpq-dev &&\
    apt-get clean

