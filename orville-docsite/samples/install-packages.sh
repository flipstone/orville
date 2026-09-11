# SNIPPET: hidden
set -e
# SNIPPET: installLibPqClient
apt update
apt install -y libpq-dev
# SNIPPET: hidden
# ssl-cert's postinst runs make-ssl-cert, which shells out to hostname; the
# hardened base image does not ship it, and without it the postinst exits
# 127 and leaves postgresql unconfigured.
apt install -y hostname
# The dhi.io mirror also carries a DHI-native postgresql-17 build (17.11-1)
# that ships its own /usr/bin/pg_config and sorts above the Debian-style
# builds, so a bare `apt install postgresql` picks it and dpkg refuses to
# overwrite the pg_config that libpq-dev already installed. Pin the server to
# the exact build libpq-dev resolved to; both come from the same source
# package, so this tracks patch releases on its own.
apt install -y postgresql "postgresql-17=$(dpkg-query -W -f='${Version}' libpq-dev)"
sed \
  -i \
  "s/#listen_addresses = 'localhost'/listen_addresses = 'localhost' /" \
  /etc/postgresql/17/main/postgresql.conf
service postgresql start
# su and runuser both @include the common-* PAM files, which the hardened
# base image omits, so either aborts here. setpriv drops privileges without
# touching PAM.
echo "ALTER USER postgres PASSWORD 'postgres'" |
  setpriv --reuid=postgres --regid=postgres --clear-groups psql
