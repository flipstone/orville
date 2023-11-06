---
title: Using Plans
navOrder: 3
---

You'll need a Haskell project named `using-plans` for this tutorial.
The setup is identical to the setup in [Getting Started](getting-started.html)
aside from the package name, so we'll avoid explaining it again here.

This example shows Plans. A Plan contains one or more SQL select statements,
and when the first is run, its result is passed into the next.

The goal of plans is to prevent [n+1
queries](https://secure.phabricator.com/book/phabcontrib/article/n_plus_one/),
while still allowing a plan for a single item to be modified to execute against
many items.

The code contains a detailed description, this document will just focus on
examples.

This example has a table with students, and a table with classes they can take.

Because multiple students can be enrolled in multiple classes, it is a
many-to-many relationship, which can be modelled with a separate table with two
foreign keys on it.

These tables are defined in a manner similar to previous tutorials. Note the
foreign keys on the `student_class` table. Begin your `src/Main.hs` like this:

$sample("using-plans/src/Main.hs", "moduleHeader")$

The three tables are declared, we'll proceed to the `main` function, which
ensures some sample data is available, and then queries it in different ways.

Since we have foreign key constraints, we have to create and delete the data in
an order that doesn't violate these constraints. Orville will throw a Haskell
runtime exception upon execution of an SQL statement that violates a
constraint.

$sample("using-plans/src/Main.hs", "mainFunction")$

Now that the data is available, let's first show off a simple plan which is
already more powerful than `findEntity`. `findMaybeOne` takes the table, and
the field to filter on, where as `findEntity` only works using the primary key.

The plan returned by `findMaybeOne` is like a parameterized query, it doesn't
yet contain the actual value to filter for.

But upon execution, that value has to be provided, and it has to match the type
of the Plan. `execute` is in MonadOrville just like `findEntity`, so from here
on out, it is familiar territory.

$sample("using-plans/src/Main.hs", "findByStudentId")$

Just to demonstrate that you can filter on other fields too, let's search for a
specific student using their name. Note that Orville doesn't have any
type-level checks for whether there is an index on the field.

$sample("using-plans/src/Main.hs", "findByStudentName")$

A plan that takes a single argument and returns a single argument can be passed
into `planList` to make it work on multiple values. Note how `execute` now
takes a list instead of just a single value.

$sample("using-plans/src/Main.hs", "findByStudentNameList")$

Remember how a plan is just a list of SQL statements. This can used in a way
similar to how `JOIN` is used in SQL.

The function `chain` is used to chain these statements together. After the
first statement is executed and its result has been sent to the client, Orville
allows for manipulating the value with `focusParam` before using it in a step.

For simplicity, we'll use `findOne` here which is like `findMaybeOne`, but
throws exceptions when it fails to find something. In practice, make sure to
only use `findOne` when there are constraints or there is otherwise certainty
that a row exists.

The following plan will:
1. find a student, given a name. Since this is the first step of the plan, the
   name is the input of the plan.
1. using the ID of the student, find a row in `student_class` that matches on
   the `studentClassStudentIdField`. Note that Orville doesn't check that the
   fields are actually comparable, or that it makes sense to compare them!
1. using the ID of the `StudentClass`, find a row in `class` that matches on the `class_id`.

$sample("using-plans/src/Main.hs", "findStudentAndClass")$

Expanding on the previous example, let's find all the names of the classes they
attend, instead of just one. This also lets us avoid unsafe uses of `findOne`.
This new version doesn't throw an exception if a student doesn't attend any
classes, which would happen when the middle plan would fail. Because of the
constraint, the final plan can't fail, so that `findOne` kept.

Let's extract the `Student -> [Class]` part to its own Plan, which we'll then
use for the final expression. Note how the following definition is similar to
the last two lines of the plan above.

$sample("using-plans/src/Main.hs", "studentsToClassesPlan")$

Remember how plans solve the n+1 problem by scaling easily from a single item
to multiple items. This means that while `studentToClassesPlan` executes as two
SQL statements, `planList studentToClassesPlan`, a version that works on
multiple elements, *also* executes as two SQL statements.

You can verify this using `explain`, which shows the generated SQL. Note how
both of these lists have two SQL statements in them:

$sample("using-plans/src/Main.hs", "explainStudentsToClassesPlan")$

With that in mind, let's use `studentToClassesPlan` with `findAll` and
`planList` to get a list of classes for each matching student.

(we sort the inner lists below, such that the result is deterministic.)

$sample("using-plans/src/Main.hs", "executeStudentsToClassesPlan")$

You can build an execute this as usual:

$sample("using-plans/run.sh","buildAndExecute")$

And you should see the following output:

$sample("using-plans/expected-output.txt")$
