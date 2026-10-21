---
title: Fighting N+1 Queries with Plans
navOrder: 3
---

An N+1 query problem is a slow data access pattern. Your code runs one query
to get a list of rows. Then it runs one more query for each of those rows. For
100 rows, the database receives 101 separate queries. Each query is a round
trip: the program sends it to the database and waits for the answer. If a
round trip takes 1 millisecond, 1,000 rows cost a full second of waiting. One
query with a join or an `IN` condition, such as
`SELECT * FROM authors WHERE id IN (1, 2, 3)`, gives the same data in a single
round trip.

This page explains why the N+1 problem is so easy to create, and what causes
it. It then explains how you can use Orville's `Plan` API to stop yourself
from accidentally creating N+1 problems in the first place. The
[Using Plans](../tutorials/using-plans.html) tutorial shows the API step by
step. This page explains why plans are structured the way that they are.

The sample code for this page uses three tables: authors, books, and
reviews. Each book has one author, and each book has zero or more reviews.
You can skim the table definitions and the `main` function for now. The
N+1 problem starts in the next section.

$sample("fighting-n-plus-one/src/Main.hs", "moduleHeader", "tableDefinitions")$

The `main` function creates the tables and adds two authors, five books, and
six reviews that will be used later:

$sample("fighting-n-plus-one/src/Main.hs", "mainFunction")$

## How an N+1 problem starts

N+1 problems happen when you loop over a list of entities and execute a
query for each one. Usually the second query is to load some other entity
that is related to the first one. In some object-relational mappers (ORMs)
this second query can hide behind normal field access. In Python, the code
`book.author.name` looks like a read from memory, but the ORM can send
a query to the database to load `author`. Access a field like this in a
`for` loop over all books and the ORM will run a separate query for each
book in the list.

In Orville, entities are normal Haskell records, so accessing a record
field will never cause a database query to be run. Queries can only be run
within a `MonadOrville` monad, so a function can only run queries if the type
makes it evident. This makes queries easier to see, but doesn't eliminate N+1
queries. The code below gets all the books, and then it gets the author of each
book, one book at a time:

$sample("fighting-n-plus-one/src/Main.hs", "naiveAuthorNames")$

To help demonstrate the problem, we will need to be able to count how
many queries the code is executing. We'll use Orville's SQL execution
callbacks to do this. Orville calls the callback for each SQL statement
that it sends. The callback adds one to a counter for each `SELECT`:

$sample("fighting-n-plus-one/src/Main.hs", "countSelectQueries")$

Running the code above with the counter shows how many queries it sends:

$sample("fighting-n-plus-one/src/Main.hs", "runNaiveAuthorNames")$

With five books, the loop above executes six `SELECT` queries: one
to get the list of books and then five (one for each book) to get the
authors. With our small set of test data this is fast. In production,
however, there could be thousands of books and this code will run
thousands of queries to fetch all their authors.

## The ability to loop is the source of the problem

The core issue is that we were able to loop over the results of the first
query and run another query based on each individual result. This is
not specific to our Haskell code in any way. Even PL/pgSQL, the language
built into PostgreSQL, lets us write a `FOR` loop that runs a query for
each row of another query:

```sql
FOR book IN SELECT * FROM books LOOP
  RETURN QUERY SELECT name FROM authors WHERE id = book.author_id;
END LOOP;
```

The cause of the N+1 problem is one program capability: the program can
look at the rows from one query and decide which queries to run next. A
monad gives you exactly this capability. Monads are what make `do` blocks
work in Haskell, and they let each step see the result of the previous
step and choose what to do next. In a `MonadOrville` block, the result of
one query is a normal Haskell value. You can loop over it, pattern match
on it, or recurse on it. Code review and discipline are the only
protection. Each `forM`, each helper function that runs a query, and each
call to that helper from a loop is a possible N+1 problem.

One way to remove the problem is to remove this capability from
the program. The Acadia query language prevents N+1 queries by being
too restrictive to express them at all. Evan Czaplicki describes it in
[Solving the 1+N Query
Problem](https://acadia.engineering/blog/solving-the-1-plus-N-query-problem).

Orville cannot remove loops from Haskell. Instead, it has a more restrictive
language inside Haskell for loading related data. That language is `Plan`.

## A plan works on one input or on many inputs

A `Plan scope param result` is a description of queries. It is not an action.
When you execute it, it takes a `param` value and produces a `result` value.
This plan finds the author of a book, using functions introduced in the
[Using Plans](../tutorials/using-plans.html) tutorial:

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

## Why a plan cannot loop over query results

A plan is safe from N+1 queries because of the things that it does not let
you do. The types make code that could cause an N+1 query impossible to
compile.

`Plan` has `Functor` and `Applicative` instances but no `Monad` instance.
`Applicative` lets you combine steps that do not depend on each other's
results. `Monad` would let a step look at an earlier result and decide what
to run next. If `Plan` had a `Monad` instance, you would be able to use
functions like `mapM` to loop and introduce an N+1 query.

To use the result of one step in a later step, you use `Plan.bind`. It is
spiritually similar to the bind operator (`>>=`) from `Monad`, and
`PlanSyntax.do` from `Orville.PostgreSQL.Plan.Syntax` uses GHC's
`QualifiedDo` extension so that a `do` block calls `Plan.bind` instead of
`>>=`. The difference is that the value `bind` gives you is not the result.
It is a `Planned scope param a` value, which represents the result. When the
plan runs for one input, the `Planned` value contains one result. When it runs
for many inputs, it contains one result for each input. When `Plan.explain`
looks at the plan, it contains no data at all.

Your code can never tell whether a `Planned` value has one or many results.
Because of this, `Planned` supports only two operations. You can change
the values inside it with `fmap`, and you can give it to a later step.
`Plan.use` makes it the result of a step, and `Plan.using` makes it the
input of another plan. You cannot pattern match on it, and
you cannot loop over it. The next step of the plan cannot depend on the
data in a result. It can only pass that data to a later step, which uses
it in the SQL condition of its query.

This code does not compile, because `books` is a `Planned` value and not a
list:

```haskell
-- This does not compile.
reviewsOfEachBook :: Plan.Plan scope AuthorId [[Review]]
reviewsOfEachBook = PlanSyntax.do
  books <- Plan.findAll bookTable bookAuthorIdField
  traverse (\book -> ...) books
```

Instead, you give `books` to a plan written for one book that has been
changed with `Plan.planList`. The `authorBibliographyPlan` example below
does this with `Plan.using`.

The `scope` type variable allows Orville to keep track internally of whether
a `Plan` is being executed with one input or many. You should never write
plans using a concrete type for `scope`. If you do, you won't be able to use
them with `Plan.planList`, because it requires its argument to work with
every scope. This means a plan that only works for one input can never be run
for many inputs by mistake.

Even though they cannot directly inspect the results of queries, plans can
still branch. `Plan.planEither` takes one plan for `Left` inputs and one
plan for `Right` inputs. With many inputs, Orville splits the inputs into
two groups and runs each plan one time for its group. `Plan.planMaybe`
and `Plan.chainMaybe` use `planEither` for optional values. If a group is
empty, Orville does not send its query.

## Plans compose

A plan is a normal Haskell value. You can give it a name, put it in a module,
and use it in larger plans. The plan below loads a book with its author and
its reviews. It uses `authorOfBookPlan` from above as one of its steps:

$sample("fighting-n-plus-one/src/Main.hs", "bookDetailsPlan")$

`Plan.explain` shows the SQL that a plan sends, without needing any
connection to the database. The sample explains the plan for one book,
and then the same plan for a list of books:

$sample("fighting-n-plus-one/src/Main.hs", "explainBookDetailsPlan")$

Both explanations contain two queries. Only the `WHERE` condition changes, from
`=` to `IN`. The number of queries comes from the structure of the plan, so you
know it before you run the plan. You could even put this number in a unit test
to catch a change that adds a query to a plan. (In case you're curious, each
query above selects the input column from its `WHERE` condition one extra time, at the
start of the column list. Orville reads that column to match each row to the
input that asked for it.)

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
Orville runs the steps in order. A plan with four steps needs four round
trips to the database. A single hand-written join can need only one. If
a step of a `Plan` must be a join, you can write the query yourself and
incorporate it into a plan with `Plan.planOperation` or `Plan.planSelect`.

Plans do not guarantee that a query is fast. A single query can be slow, e.g. if
the column in the `WHERE` condition has no index. Orville does not make sure
that an index exists. A very large input list also makes a very large `IN`
condition.

Plans do not protect code that is outside a plan. If you call
`Plan.execute` (or any other Orville function) inside a `forM` loop,
you have an N+1 problem again. You still need to be vigilant to make
sure N+1 queries don't creep in. `Plan` gives you an answer for
what to do when you find one.

## Summary

The N+1 problem comes from one source: code that uses query results to
decide which queries to run next. `Plan` does not let this happen. The
results of a step are available only as `Planned` values. Your code can
transform them and give them to later steps, but it cannot loop over them.
Because of this restriction, Orville can run a plan for one input or for many
inputs with the same number of queries.

A good rule of thumb: when you see a loop over query results whose body runs
another query, write a plan for one item instead. Then run it for all the
items with `Plan.planList`.

To learn the API step by step, read the [Using
Plans](../tutorials/using-plans.html) tutorial. For the full list of plan
functions, read the documentation of the `Orville.PostgreSQL.Plan` module on
Hackage.

To run the sample yourself, build and run it like this:

$sample("fighting-n-plus-one/run.sh", "buildAndExecute", "filename=")$

The output is:

$sample("fighting-n-plus-one/expected-output.txt", "filename=output")$
