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
apt install -y postgresql
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
