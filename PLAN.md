#!/usr/bin/env mdsh

# Using Plans

This example shows Plans. A Plan contains one or more SQL select statements,
and when the first is run, its result is passed into the next.

The goal of plans is to prevent [n+1 queries](https://secure.phabricator.com/book/phabcontrib/article/n_plus_one/),
while still allowing a plan for a single item to be modified to execute against
many items.

The code contains a detailed description, this document will just focus on
examples.

This example has a table with students, and a table with classes they can take.

Because multiple students can be enrolled in multiple classes, it is a
many-to-many relationship, which can be modelled with a separate table with two
foreign keys on it.

First let's initialize a Haskell project. This first part is identical to the
GETTING-STARTED guide, so we'll avoid explaining it here.

```shell
mkdir orville-plan
cd orville-plan
cabal init -n --exe
sed -i -re 's/build-depends:/build-depends: orville-postgresql, text,/' *.cabal
cat << 'EOF' > cabal.project
packages: .
source-repository-package
  type: git
  location: https://github.com/flipstone/orville.git
  tag: c3bdcebac4beb8ef50715439ea24562ed2b95b36
  subdir: orville-postgresql
EOF
```

These tables are defined in a manner similar to previous tutorials. Note the foreign keys on the `student_class` table.

```shell
cat << 'EOF' > app/Main.hs
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Plan as Plan

import           Data.List (sort)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Int as Int
import qualified Data.Text as T

-------------
-- Student --
-------------

type StudentId = Int.Int32
type StudentName = T.Text
type StudentAge = Int.Int32

data Student = Student
  { studentId :: StudentId
  , studentName :: StudentName
  , studentAge :: StudentAge
  }
  deriving Show

studentIdField :: O.FieldDefinition O.NotNull StudentId
studentIdField =
  O.integerField "id"

studentNameField :: O.FieldDefinition O.NotNull StudentName
studentNameField =
  O.unboundedTextField "name"

studentAgeField :: O.FieldDefinition O.NotNull StudentAge
studentAgeField =
  O.integerField "age"

studentMarshaller :: O.SqlMarshaller Student Student
studentMarshaller =
  Student
    <$> O.marshallField studentId studentIdField
    <*> O.marshallField studentName studentNameField
    <*> O.marshallField studentAge studentAgeField

studentTable :: O.TableDefinition (O.HasKey StudentId) Student Student
studentTable =
  O.mkTableDefinition "plan_demo_student" (O.primaryKey studentIdField) studentMarshaller

----------------------------
-- Student to Class Table --
----------------------------

studentClassIdField :: O.FieldDefinition O.NotNull Int.Int32
studentClassIdField =
  O.integerField "id"

studentClassClassIdField :: O.FieldDefinition O.NotNull Int.Int32
studentClassClassIdField =
  O.integerField "class_id"

studentClassStudentIdField :: O.FieldDefinition O.NotNull Int.Int32
studentClassStudentIdField =
  O.integerField "student_id"

data StudentClass = StudentClass
  { studentClassId :: Int.Int32
  , studentClassClassId :: Int.Int32
  , studentClassStudentId :: Int.Int32
  }

studentClassMarshaller :: O.SqlMarshaller StudentClass StudentClass
studentClassMarshaller =
  StudentClass
    <$> O.marshallField studentClassId studentClassIdField
    <*> O.marshallField studentClassClassId studentClassClassIdField
    <*> O.marshallField studentClassStudentId studentClassStudentIdField

studentClassTable :: O.TableDefinition (O.HasKey Int.Int32) StudentClass StudentClass
studentClassTable =
  O.addTableConstraints
    [ O.foreignKeyConstraint (O.tableIdentifier classTable) $
        O.foreignReference (O.fieldName studentClassClassIdField) (O.fieldName classIdField) :| []
    , O.foreignKeyConstraint (O.tableIdentifier studentTable) $
        O.foreignReference (O.fieldName studentClassStudentIdField) (O.fieldName studentIdField) :| []
    ]
  $ O.mkTableDefinition "plan_demo_student_class" (O.primaryKey studentClassIdField) studentClassMarshaller

-----------
-- Class --
-----------

classIdField :: O.FieldDefinition O.NotNull Int.Int32
classIdField =
  O.integerField "id"

classSubjectField :: O.FieldDefinition O.NotNull T.Text
classSubjectField =
  O.unboundedTextField "subject"

data Class = Class
  { classId :: Int.Int32
  , classSubject :: T.Text
  }
  deriving (Show, Eq, Ord)

classMarshaller :: O.SqlMarshaller Class Class
classMarshaller =
  Class
    <$> O.marshallField classId classIdField
    <*> O.marshallField classSubject classSubjectField

classTable :: O.TableDefinition (O.HasKey Int.Int32) Class Class
classTable =
  O.mkTableDefinition "plan_demo_class" (O.primaryKey classIdField) classMarshaller
EOF
```

The three tables are declared, we'll proceed to the `main` function, which
ensures some sample data is available, and then queries it in different ways.

Since we have foreign key constraints, we have to create and delete the data in
an order that doesn't violate these constraints. Orville will throw a Haskell
runtime exception upon execution of an SQL statement that violates a
constraint.

```shell
cat << 'EOF' >> app/Main.hs
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

  O.runOrville pool $ do
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable studentTable, AutoMigration.SchemaTable classTable, AutoMigration.SchemaTable studentClassTable]
    _ <- O.deleteEntity studentClassTable 0
    _ <- O.deleteEntity studentClassTable 1
    _ <- O.deleteEntity studentClassTable 2
    _ <- O.deleteEntity classTable 0
    _ <- O.deleteEntity classTable 1
    _ <- O.deleteEntity classTable 2
    _ <- O.deleteEntity studentTable 0
    _ <- O.deleteEntity studentTable 1
    _ <- O.insertEntity studentTable Student { studentId = 0, studentName = T.pack "Name", studentAge = 91 }
    _ <- O.insertEntity studentTable Student { studentId = 1, studentName = T.pack "Other Name", studentAge = 42 }
    _ <- O.insertEntity classTable Class { classId = 0, classSubject = T.pack "Painting" }
    _ <- O.insertEntity classTable Class { classId = 1, classSubject = T.pack "Cooking" }
    _ <- O.insertEntity classTable Class { classId = 2, classSubject = T.pack "Swimming" }
    _ <- O.insertEntity studentClassTable $ StudentClass {studentClassId=0, studentClassClassId=0, studentClassStudentId=0}
    _ <- O.insertEntity studentClassTable $ StudentClass {studentClassId=1, studentClassClassId=2, studentClassStudentId=0}
    _ <- O.insertEntity studentClassTable $ StudentClass {studentClassId=2, studentClassClassId=1, studentClassStudentId=1}
    pure ()
EOF
```

Now that the data is available, let's first show off a simple plan which is
already more powerful than `findEntity`. `findMaybeOne` takes the table, and
the field to filter on, where as `findEntity` only works using the primary key.

The plan returned by `findMaybeOne` is like a parameterized query, it doesn't
yet contain the actual value to filter for.

But upon execution, that value has to be provided, and it has to match the type
of the Plan. `execute` is in MonadOrville just like `findEntity`, so from here
on out, it is familiar territory.

```shell
cat << 'EOF' >> app/Main.hs
  print =<< O.runOrville pool (Plan.execute (Plan.findMaybeOne studentTable studentIdField) 0)
EOF
```

Just to demonstrate that you can filter on other fields too, let's search for a
specific student using their name. Note that Orville doesn't have any
type-level checks for whether there is an index on the field.

```shell
cat << 'EOF' >> app/Main.hs
  print =<< O.runOrville pool (Plan.execute (Plan.findMaybeOne studentTable studentNameField) (T.pack "Other Name"))
EOF
```

A plan that takes a single argument and returns a single argument can be passed into `planList` to make it work on multiple values.
Note how `execute` now takes a list instead of just a single value.

```shell
cat << 'EOF' >> app/Main.hs
  print =<< O.runOrville pool (Plan.execute (Plan.planList (Plan.findMaybeOne studentTable studentNameField)) [T.pack "Other Name", T.pack "Name"])
EOF
```

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

```shell
cat << 'EOF' >> app/Main.hs
  (print =<<) . O.runOrville pool $ Plan.execute
    (              Plan.findOne studentTable studentNameField
      `Plan.chain` Plan.focusParam studentId (Plan.findOne studentClassTable studentClassStudentIdField)
      `Plan.chain` Plan.focusParam studentClassClassId (Plan.findOne classTable classIdField)
    )
    (T.pack "Other Name")
EOF
```

Expanding on the previous example, let's find all the names of the classes they
attend, instead of just one. This also lets us avoid unsafe uses of `findOne`.
This new version doesn't throw an exception if a student doesn't attend any
classes, which would happen when the middle plan would fail. Because of the
constraint, the final plan can't fail, so that `findOne` kept.

Let's extract the `Student -> [Class]` part to its own Plan, which we'll then
use for the final expression. Note how the following definition is similar to
the last two lines of the plan above.

```shell
cat << 'EOF' >> app/Main.hs
  let
    studentToClassesPlan :: Plan.Plan scope Student [Class]
    studentToClassesPlan =
                   Plan.focusParam studentId (Plan.findAll studentClassTable studentClassStudentIdField)
      `Plan.chain` Plan.planList (Plan.focusParam studentClassClassId $ Plan.findOne classTable classIdField)
EOF
```

Remember how plans solve the n+1 problem by scaling easily from a single item
to multiple items. This means that while `studentToClassesPlan` executes as two
SQL statements, `planList studentToClassesPlan`, a version that works on
multiple elements, *also* executes as two SQL statements.

You can verify this using `explain`, which shows the generated SQL. Note how
both of these lists have two SQL statements in them:

```shell
cat << 'EOF' >> app/Main.hs
  print $ Plan.explain studentToClassesPlan
  print $ Plan.explain (Plan.planList studentToClassesPlan)
EOF
```

With that in mind, let's use `studentToClassesPlan` with `findAll` and
`planList` to get a list of classes for each matching student.

(we sort the inner lists below, such that the result is deterministic.)

```shell
cat << 'EOF' >> app/Main.hs
  (print =<<) . fmap (fmap sort) . O.runOrville pool $ Plan.execute
    ( Plan.findAll studentTable studentNameField
      `Plan.chain` Plan.planList studentToClassesPlan
    )
    (T.pack "Name")
EOF
```

See the line above EOF below for the results of this final query.

```shell
cabal build
cat << 'EOF' > plan-test.t
$ cd $TESTDIR
$ cp $(cabal list-bin exe:orville-plan | tail -n1) $OLDPWD
$ cd $OLDPWD
$ ./orville-plan
Just (Student {studentId = 0, studentName = "Name", studentAge = 91})
Just (Student {studentId = 1, studentName = "Other Name", studentAge = 42})
[Just (Student {studentId = 1, studentName = "Other Name", studentAge = 42}),Just (Student {studentId = 0, studentName = "Name", studentAge = 91})]
Class {classId = 1, classSubject = "Cooking"}
["SELECT \"student_id\",\"id\",\"class_id\",\"student_id\" FROM \"plan_demo_student_class\" WHERE (\"student_id\") = ($1)","SELECT \"id\",\"id\",\"subject\" FROM \"plan_demo_class\" WHERE (\"id\") IN ($1, $2)"]
["SELECT \"student_id\",\"id\",\"class_id\",\"student_id\" FROM \"plan_demo_student_class\" WHERE (\"student_id\") IN ($1, $2)","SELECT \"id\",\"id\",\"subject\" FROM \"plan_demo_class\" WHERE (\"id\") IN ($1, $2)"]
[[Class {classId = 0, classSubject = "Painting"},Class {classId = 2, classSubject = "Swimming"}]]
EOF
~/.local/bin/prysk plan-test.t --indent=0
```
