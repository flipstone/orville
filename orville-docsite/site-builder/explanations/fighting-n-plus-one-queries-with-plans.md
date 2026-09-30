---
title: Fighting N+1 Queries with Plans
navOrder: 3
---

An N+1 query problem is a slow data access pattern. Your code runs one query
to get a list of rows. Then it runs one more query for each of those rows. For
100 rows, the database receives 101 separate queries. One query with a join or
an `IN` condition gives the same data in a single round trip.

This page explains why the N+1 problem is so easy to create, and what causes
it. It then explains how Orville's `Plan` API removes the cause. The
[Using Plans](../tutorials/using-plans.html) tutorial shows the API step by
step. This page explains why plans have the shape that they have.

All code on this page comes from one sample program. It uses three tables:
authors, books, and reviews. Each book has one author, and each book has zero
or more reviews.

$sample("fighting-n-plus-one/src/Main.hs", "moduleHeader", "tableDefinitions")$

The `main` function creates the tables and adds two authors, five books, and
six reviews:

$sample("fighting-n-plus-one/src/Main.hs", "mainFunction")$

## How an N+1 problem starts

In many object-relational mappers (ORMs), the problem hides behind normal
field access. In Python, the code `book.author.name` looks like a read from
memory. But the ORM can send a query to the database to load `author`. If you
put that line in a `for` loop over all books, you get one query for the books
and one query for each book. Nothing in the code tells you this. GraphQL
resolvers have the same risk, because one field of a result can require a
query of its own.

If you access a record field, Orville does not load data. A `Book` is a plain
Haskell record, and `bookAuthorId` is a plain function. Every query is an
explicit call in a `MonadOrville` monad. This makes queries easier to see, but
it does not stop N+1 queries. The code below gets all the books, and then it
gets the author of each book, one book at a time:

$sample("fighting-n-plus-one/src/Main.hs", "naiveAuthorNames")$

To count the queries, the sample adds a SQL execution callback. Orville calls
the callback for each SQL statement that it sends. The callback adds one to a
counter for each `SELECT`:

$sample("fighting-n-plus-one/src/Main.hs", "countSelectQueries")$

$sample("fighting-n-plus-one/src/Main.hs", "runNaiveAuthorNames")$

With five books, the loop sends six `SELECT` queries: one for the books and
five for the authors. The code is correct, and with test data it is fast. With
production data, the number of queries grows with the number of rows.

## The cause is the loop, not the objects

The same problem occurs in languages that have no objects. PL/pgSQL runs
inside PostgreSQL and has no ORM, but a `FOR` loop can still run a query for
each row of another query:

```sql
FOR book IN SELECT * FROM books LOOP
  RETURN QUERY SELECT name FROM authors WHERE id = book.author_id;
END LOOP;
```

The cause of the N+1 problem is one capability: the program can look at the
rows from one query and decide which queries to run next. A monad gives you
exactly this capability. In a `MonadOrville` block, the result of one query is
a normal Haskell value. You can loop over it, pattern match on it, or recurse
on it. Code review and discipline are the only protection. Each `forM`, each
helper function that runs a query, and each call to that helper from a loop is
a possible N+1 problem.

One way to remove the problem is to remove the capability. The Datalog query
language is an example of this idea. Datalog removes unbounded recursion from
Prolog. As a result, every Datalog query terminates in polynomial time. A
language with less power gives stronger guarantees. The Acadia query language
uses the same idea to prevent N+1 queries. Evan Czaplicki describes it in
[Solving the 1+N Query
Problem](https://acadia.engineering/blog/solving-the-1-plus-N-query-problem).

Orville cannot remove loops from Haskell. Instead, it gives you a smaller
language inside Haskell for loading related data. That language is `Plan`.

## A plan works on one input or on many inputs

A `Plan scope param result` is a description of queries. It is not an action.
When you execute it, it takes a `param` value and produces a `result` value.
This plan finds the author of a book:

$sample("fighting-n-plus-one/src/Main.hs", "authorOfBookPlan")$

The plan is written for one book. `Plan.planList` changes it into a plan for a
list of books. The new plan does not run the original plan once per book.
Orville runs each query of the plan one time, with an `IN` condition that
contains the values from all the inputs. Then it matches each result row to
the input that asked for it.

$sample("fighting-n-plus-one/src/Main.hs", "runPlannedAuthorNames")$

This version sends two `SELECT` queries: one for the books and one for the
authors of all the books. With 5,000 books, it still sends two queries. The
results come back in the same order as the inputs. If two books have the same
author, each book still gets its own result.

The books query in this example is not part of a plan. You do not need to
write your whole program with plans. You can use a plan at the place where
code starts to load related data for many rows.

## Why a plan cannot become a loop

A plan is safe because of the things that it does not let you do. The types
make the restriction, so the compiler enforces it.

`Plan` has `Functor` and `Applicative` instances. It has no `Monad` instance.
To use the result of one step in a later step, you use `Plan.bind`, or the
`do` notation from `Orville.PostgreSQL.Plan.Syntax`. The value that `bind`
gives you is not the result. It is a `Planned scope param a` value, which
represents the result. When the plan runs for one input, the `Planned` value
contains one result. When it runs for many inputs, it contains one result for
each input. When `Plan.explain` looks at the plan, it contains no data at all.

Your code does not know which of these cases applies. Because of this,
`Planned` supports only two operations. You can change the values inside it
with `fmap`, and you can give it to a later step with `Plan.use` or
`Plan.using`. You cannot pattern match on it, and you cannot loop over it. The
next step of the plan cannot depend on the data in a result. It can only
receive that data as input.

This code does not compile, because `books` is a `Planned` value and not a
list:

```haskell
-- This does not compile.
authorsOneByOne :: Plan.Plan scope AuthorId [Author]
authorsOneByOne = PlanSyntax.do
  books <- Plan.findAll bookTable bookAuthorIdField
  traverse (\book -> ...) books
```

The `scope` type variable closes one more gap. `Plan.planList` and
`Plan.planMany` accept only a plan that works for every `scope`. A plan that
you wrote for exactly one input cannot be given to them. Thus each plan that
you can use with many inputs is also correct with many inputs.

Plans can still branch. `Plan.planEither` takes one plan for `Left` inputs and
one plan for `Right` inputs. With many inputs, Orville splits the inputs into
two groups and runs each plan one time for its group. `Plan.planMaybe` and
`Plan.chainMaybe` use `planEither` for optional values. If a group is empty,
Orville does not send its query.

## Plans compose

A plan is a normal Haskell value. You can give it a name, put it in a module,
and use it in larger plans. The plan below loads a book with its author and
its reviews. It uses `authorOfBookPlan` from above as one of its steps:

$sample("fighting-n-plus-one/src/Main.hs", "bookDetailsPlan")$

`Plan.explain` shows the SQL that a plan sends, without a connection to the
database. The sample explains the plan for one book, and then the same plan
for a list of books:

$sample("fighting-n-plus-one/src/Main.hs", "explainBookDetailsPlan")$

Both explanations contain two queries. Only the `WHERE` condition changes,
from `=` to `IN`. The number of queries comes from the structure of the plan,
so you know it before you run the plan. You can also put this number in a unit
test. If a change adds a query to a plan, the test fails.

Plans also nest. The next plan loads an author, the books of that author, and
the details of each book. The inner step is `Plan.planList bookDetailsPlan`:

$sample("fighting-n-plus-one/src/Main.hs", "authorBibliographyPlan")$

The sample executes this plan for two authors. It prints each book with a
small helper function:

$sample("fighting-n-plus-one/src/Main.hs", "describeBook", "runAuthorBibliographyPlan")$

The plan sends four queries: authors, books, authors of the books, and
reviews. A version with `forM` loops sends 14 queries for the same data. That
is two queries for each author and two queries for each book. With more data,
the loop version sends more queries. The plan version still sends four.

## What plans do not promise

Plans give you one specific guarantee. A plan sends at most one query for each
step in its structure, and the number of rows does not change this number.
Some other things are not guaranteed.

Orville does not make joins from plans. Each step is a separate query, and
Orville runs the steps in order. A plan with four steps needs four round trips
to the database. A single hand-written join can need only one. If a step must
be a join, you can write the query yourself and add it to a plan with
`Plan.planOperation` or `Plan.planSelect`.

Plans do not guarantee that a query terminates or that it is fast. A plan is a
Haskell value, and Haskell lets you define a plan that refers to itself
without end. A single query can also be slow, e.g. if the column in the
`WHERE` condition has no index. Orville does not make sure that an index exists. A very large
input list also makes a very large `IN` condition.

Plans do not protect code that is outside a plan. If you call `Plan.execute`
inside a `forM` loop, you have an N+1 problem again. But the place that you
must examine is smaller: the calls to `execute`, and not every function that
loads data.

## Summary

The N+1 problem comes from one capability: code that uses query results to
decide which queries to run next. A `Plan` does not have this capability. The
results of a step are available only as `Planned` values. Your code can
transform them and give them to later steps, but it cannot loop over them.
Because of this restriction, Orville can run a plan for one input or for many
inputs with the same number of queries.

To learn the API step by step, read the [Using
Plans](../tutorials/using-plans.html) tutorial. For the full list of plan
functions, read the documentation of the `Orville.PostgreSQL.Plan` module on
Hackage.

To run the sample yourself, build and run it like this:

$sample("fighting-n-plus-one/run.sh", "buildAndExecute", "filename=")$

The output is:

$sample("fighting-n-plus-one/expected-output.txt", "filename=output")$
