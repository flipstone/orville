-- SNIPPET: moduleHeader
module Main
  ( main
  ) where

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
-- SNIPPET: mainFunction
main :: IO ()
main = do
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "host=localhost user=postgres password=postgres"
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
-- SNIPPET: findByStudentId
  print =<< O.runOrville pool (Plan.execute (Plan.findMaybeOne studentTable studentIdField) 0)
-- SNIPPET: findByStudentName
  print =<< O.runOrville pool (Plan.execute (Plan.findMaybeOne studentTable studentNameField) (T.pack "Other Name"))
-- SNIPPET: findByStudentNameList
  print =<< O.runOrville pool (Plan.execute (Plan.planList (Plan.findMaybeOne studentTable studentNameField)) [T.pack "Other Name", T.pack "Name"])
-- SNIPPET: findStudentAndClass
  (print =<<) . O.runOrville pool $ Plan.execute
    (              Plan.findOne studentTable studentNameField
      `Plan.chain` Plan.focusParam studentId (Plan.findOne studentClassTable studentClassStudentIdField)
      `Plan.chain` Plan.focusParam studentClassClassId (Plan.findOne classTable classIdField)
    )
    (T.pack "Other Name")
-- SNIPPET: studentsToClassesPlan
  let
    studentToClassesPlan :: Plan.Plan scope Student [Class]
    studentToClassesPlan =
                   Plan.focusParam studentId (Plan.findAll studentClassTable studentClassStudentIdField)
      `Plan.chain` Plan.planList (Plan.focusParam studentClassClassId $ Plan.findOne classTable classIdField)
-- SNIPPET: explainStudentsToClassesPlan
  print $ Plan.explain studentToClassesPlan
  print $ Plan.explain (Plan.planList studentToClassesPlan)
-- SNIPPET: executeStudentsToClassesPlan
  (print =<<) . fmap (fmap sort) . O.runOrville pool $ Plan.execute
    ( Plan.findAll studentTable studentNameField
      `Plan.chain` Plan.planList studentToClassesPlan
    )
    (T.pack "Name")
