---
title: Getting Started
navOrder: 0
---

Orville is a PostgreSQL client library, so it needs a working PostgreSQL server
to work. This tutorial assumes that PostgreSQL in running on `localhost`,
listening on port `5432` and that the user `postgres` exists with password
`postgres`.

Orville itself depends on the native LibPQ library being installed to
communicate with the server, so we need to install it before building
everything. If you're on Debian-like distribution that uses `apt`, this command
will do that.

$sample("install-packages.sh", "installLibPqClient", "filename=")$

With that setup out of the way, let's make a new project for this demo:

$sample("getting-started/run.sh", "initProject", "filename=")$

Now we need to edit `stack.yaml` and add `orville-postgresql` as an extra-dep.

$sample("getting-started/add-orville-extra-dep-to-stack-yaml.patch", "filename=stack.yaml")$

Next, edit the `orville-getting-started.cabal` file to make it depend on
Orville.

$sample("getting-started/add-orville-to-cabal-file.patch", "filename=orville-getting-started.cabal")$

Here is a minimal program that simply computes 1+1 on the database. Replace
the `src/Main.hs` with it.

$sample("getting-started/Main.hs", "filename=src/Main.hs")$

All that's left is to build the executable and run it. When it runs it will
connect to the PostgreSQL server, run the SQL to calculate `1 + 1` and then
print the result out.

$sample("getting-started/run.sh", "buildAndExecute", "filename=")$

This is the output you will see in the console if everything went as planned:

$sample("getting-started/expected-output.txt", "filename=output")$
