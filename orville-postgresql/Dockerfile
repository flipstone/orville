FROM flipstone/stack:v2-1.9.3

# The fpco.list apt source was causing an error build:
#   E: The method driver /usr/lib/apt/methods/https could not be found.
#
# The docker image above is no longer officially supported. Probably FPCO
# switched to https for their apt repo and put a redirect in place, but the
# base docker image does not have the apt https driver installed to handle that
# redirect. Since we expect the `0.9_development` branch to replace master, I've
# simply removed the offending file from the apt sources below.

RUN rm /etc/apt/sources.list.d/fpco.list &&\
    apt-get update &&\
    apt-get install -y --no-install-recommends libpq5 libpq-dev &&\
    apt-get clean

