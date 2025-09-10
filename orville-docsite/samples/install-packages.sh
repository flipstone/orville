# SNIPPET: hidden
set -e
# SNIPPET: installLibPqClient
apt update
apt install -y libpq-dev
# SNIPPET: hidden
apt install -y postgresql
sed \
  -i \
  "s/#listen_addresses = 'localhost'/listen_addresses = 'localhost' /" \
  /etc/postgresql/17/main/postgresql.conf
service postgresql start
echo "ALTER USER postgres PASSWORD 'postgres'" | su postgres -c psql
