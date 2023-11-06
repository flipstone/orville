---
title: Using JSON
navOrder: 4
---

You'll need a Haskell project named `using-json` for this tutorial.
The setup is identical to the setup in [Getting Started](getting-started.html)
aside from the package name, so we'll avoid explaining it again here.

SQL has a rigid schema. Using JSON inside SQL allows for pockets of schemaless
data, that is still queryable using PostgreSQL's built-in functionality.

This document explains how to use Orville with the JSONB data type that
PostgreSQL natively supports.

Project initialization is similar to previous tutorials, but additional
dependencies like Aeson have been added. Aeson is a JSON library for Haskell.

Here's the beginning of our `src/Main.hs` file.

$sample("using-json/src/Main.hs", "moduleHeader")$

Let's suppose we have an example entity with an ID, and some arbitrary JSON
data in a column called 'tags'.

Note how `fooTagsField` below uses the `Value` type from the Aeson library.

Remember that the `Value` contains its own `Null` constructor, which is
distinct from SQL's `NULL`. So we can have JSON nulls in this field, but no SQL
nulls.

We could also use a custom type with `FromJSON`/`ToJSON` instances, since
`jsonb` allows for that too. Aeson is not the focus of this document though.

$sample("using-json/src/Main.hs", "dataTypes")$

Before we can define the corresponding `SqlMarshaller`, we'll need to define the
`aesonValueField` helper function. This is done `tryConvertSqlType` along with
`jsonb` field to apply Aeson encoding and decode.

$sample("using-json/src/Main.hs", "aesonValueField")$

Let's define the `SqlMarshaller` and the table. This is standard stuff, no
surprises here.

$sample("using-json/src/Main.hs", "tableDefinition")$

With all definitions done, we can write `main`. Orville will also use the parts
of the `SqlType` during migration.

$sample("using-json/src/Main.hs", "mainFunction")$

We'll construct a JSON value using the Aeson library, which makes this look
fairly verbose. But imagine that the `Array` value below was read from a file,
or received over HTTP from a web browser.

$sample("using-json/src/Main.hs", "insertEntity")$

Using raw SQL, we can use PostgreSQL's built-in JSONB functions. Let's suppose
we want a row returned for each of the values in the `Array` above.

| ID | Tag |
| -- | --- |
| 0  | 1   |
| 0  | 2   |
| 0  | 3   |

We can use an `SqlMarshaller` to produce a result like this, even though there
is no table for the returned schema. The programmer must ensure correspondence
of the SQL and the `SqlMarshaller`. If they don't match, an exception will be
thrown.

We'll have the `SqlMarshaller` work with tuples and `marshallReadOnlyField`s.
These allow for succintly defining a quick one-off `SqlMarshaller`.

$sample("using-json/src/Main.hs", "selectJSONArray")$

# Program output and test

This concludes this tutorial. You can build an execute this as usual:

$sample("using-json/run.sh","buildAndExecute")$

And you should see the following output:

$sample("using-json/expected-output.txt")$
