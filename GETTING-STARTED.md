#!/usr/bin/env mdsh

# Getting started with Orville

Orville is a PostgreSQL client library, so it needs a working server to work. A working setup is assumed in this document.

For this demo, let's make a new project:

```shell
mkdir orville-getting-started
cd orville-getting-started
cabal init -n --exe
```

Make it depend on Orville.

```shell
sed -i -re 's/build-depends:/build-depends: orville-postgresql ^>=1.0.0.0,/' *.cabal
```

Here is a minimal program that simply computes 1+1 on the database:

```shell
cat <<EOF > app/Main.hs
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Raw.Connection as Connection
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

main :: IO ()
main = do
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "host=pg user=orville_docs password=orville"
          , O.connectionNoticeReporting = O.DisableNoticeReporting
          , O.connectionPoolStripes = O.OneStripePerCapability
          , O.connectionPoolLingerTime = 10
          , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
          }

  Connection.withPoolConnection pool $ \connection -> do
    result <- RawSql.execute connection (RawSql.fromString "SELECT 1+1")
    [[(_, sqlValue)]] <- Execution.readRows result
    print (SqlValue.toInt sqlValue)
EOF
```

```shell
apt -y install libpq-dev
```

We need to update the package index for Cabal to find orville

```shell
cabal update
```

Now, we can finally compile the demo project:

```shell
cabal build
```

The expected output is shown below:

```shell
cat <<EOF > readme-test.t
$ cd \$TESTDIR
$ cp \$(cabal list-bin exe:orville-getting-started | tail -n1) \$OLDPWD
$ cd \$OLDPWD
$ ./orville-getting-started
Right 2
EOF
```

It can be verified to match using `prysk` (which can be installed using `pip3`):

```shell
~/.local/bin/prysk  readme-test.t --indent=0
```
