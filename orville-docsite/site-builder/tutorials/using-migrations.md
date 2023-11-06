---
title: Using Migrations
navOrder: 2
---

## Adding a new non-nullable column

We'll show how, to add a new non-nullable column:

1. first, we create the initial version of the table without the 'new' column
1. we then add the column, but only as *nullable*
1. we make sure all values are present on the nullable column
1. we then migrate the column to be non-nullable

You'll need a Haskell project named `using-migrations` for this tutorial.
The setup is identical to the setup in [Getting Started](getting-started.html)
aside from the package name, so we'll avoid explaining it again here. Instead
we'll get straight on creating our `src/Main.hs` file.

$sample("using-migrations/src/Main.hs", "moduleHeaderAndTypes")$

Note how the following tables have the same SQL table names. Imagine that
`table1` is the initial version of the table, which then is changed to
`table2`, and so on. In practice, they can keep their Haskell names, since they
won't need to co-exist, like they do in this document.

Orville's AutoMigration tool sees that the difference between the tables is,
that the second table has an additional column, and it will generate and
execute the DDL to add the column. It needs to be nullable, because the database won't
have any values for it when it is added.

$sample("using-migrations/src/Main.hs", "tableDefinitions")$

Now let's add a `main` function invokes Orville's auto-migration tool to
demonstrate making changes to the table.

$sample("using-migrations/src/Main.hs", "mainFunction")$

# Dropping a column

Orville won't automatically drop a column in the SQL database that isn't in the SqlMarshaller.
The column will just be ignored.

We have to tell Orville explicitly about the columns that are safe to drop.
This is done using the `dropColumns` combinator. Add this to the bottom of the
`main` function:

$sample("using-migrations/src/Main.hs", "droppingColumns")$

# Conclusion

This concludes this tutorial. You can build an execute this as usual:

$sample("using-migrations/run.sh","buildAndExecute")$

And you should see the following output:

$sample("using-migrations/expected-output.txt")$
