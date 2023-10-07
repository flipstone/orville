#!/usr/bin/env mdsh

# Getting started with Orville

Orville is a PostgreSQL client library, so it needs a working server to work. A working setup is assumed in this document.

For this demo, let's make a new project:

```shell
mkdir orville-getting-started
cd orville-getting-started
cabal init -n --exe
```

Make it depend on Orville (the LibPQ variant, which is preferred!), resource-pool and bytestring, which Orville integrates with:

```shell
sed -i -re 's/build-depends:/build-depends: orville-postgresql, bytestring, resource-pool,/' *.cabal
```

Here is a minimal program that simply computes 1+1 on the database:

```shell
cat <<EOF > app/Main.hs
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import qualified Data.Pool as Pool
import           Data.String (IsString(fromString))

main :: IO ()
main = do
  pool <- O.createConnectionPool O.DisableNoticeReporting 1 10 1 (fromString "host=pg user=orville_docs password=orville")
  Pool.withResource pool $ \connection -> do
    result <- RawSql.execute connection (RawSql.fromString "SELECT 1+1")
    [[(_, sqlValue)]] <- Execution.readRows result
    print (SqlValue.toInt sqlValue)
EOF
```

We need to tell Cabal how to find Orville:

```shell
cat << EOF > cabal.project
packages: .
source-repository-package
  type: git
  location: https://github.com/flipstone/orville.git
  tag: 82fc9d4d93a24440fe3c9d34a75a4a83acde131b
  subdir: orville-postgresql
EOF
```

To compile Orville, you need the libpq headers installed. On Debian-based distributions, this can be done using:

```shell
apt -y install libpq-dev
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
