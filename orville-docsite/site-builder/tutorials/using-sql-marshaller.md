---
title: Using SqlMarshaller
navOrder: 1
---

The SQL Marshaller helps with converting from a Haskell record to SQL and back.
This document also shows how the SQL marshaller in table definitions, which can
then be used to generate Data Definition Language (DDL) queries to create
tables in the database.

You'll need a Haskell project named `using-sql-marshaller` for this tutorial.
The setup is identical to the setup in [Getting Started](getting-started.html)
aside from the package name, so we'll avoid explaining it again here. Instead
we'll get straight on creating our `src/Main.hs` file.

First, let's import the necessary modules. We use less 'internal' imports here
than in the [Getting Started](getting-started.html) guide, because we are using
a higher abstraction level.

$sample("using-sql-marshaller/src/Main.hs","moduleHeader")$

Next, let's declare some type aliases and the Haskell record itself. We'll
enable storing values of this record in the SQL database.

$sample("using-sql-marshaller/src/Main.hs","dataTypes")$

To store the record in SQL, we need to define Orville FieldDefinitions for each
of the fields of the record. A FieldDefinition maps to a column in SQL. Also
note how the type is parameterized on whether it is nullable or not.

The strings passed in here are the actual SQL column names. FieldDefinitions
help avoiding typos, since the Haskell compiler will fail compilation if the
name of a FieldDefinition is misspelt.

$sample("using-sql-marshaller/src/Main.hs","fieldDefinitions")$

Now that the fields are defined, we can use them, togehter with the record
field selector functions, to define the `SqlMarshaller`.

$sample("using-sql-marshaller/src/Main.hs","sqlMarshaller")$

We can use the marshaller to define the Orville table definition. This binding
represents a table, but note that it doesn't necessarily exist in the SQL
database until we start using it.

$sample("using-sql-marshaller/src/Main.hs","tableDefinition")$

Now let's write the main function, which does the following:
1. auto migrates using the table defintion. It will match the Haskell record at
   the time of execution of the statement. The details of migration are out of
   scope for this article.
1. deletes the Foo with ID 0, if it exists. Necessary to allow the program to
   be repeatedly executed, as primary keys can't be duplicated.
1. inserts an example Foo object with ID 0.
1. reads it back out using its ID 0. If an entity with the given ID doesn't
   exist, we'd get a Nothing here.
1. prints the retrieved entity

$sample("using-sql-marshaller/src/Main.hs","mainFunction")$

The program is now complete, let's compile and run!

$sample("using-sql-marshaller/run.sh","buildAndExecute")$

Once it builds and runs successfully you should see the following output:

$sample("using-sql-marshaller/expected-output.txt")$
