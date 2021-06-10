module Main where

import Control.Arrow
import Control.Exception
import Control.Monad (void)
import Data.Convertible (convert)
import qualified Data.Map.Strict as Map
import Data.Text (pack, unpack)
import qualified Data.Text.IO as T
import qualified Database.HDBC.PostgreSQL as Postgres
import System.Environment (getEnv)

import qualified Database.Orville.PostgreSQL as O
import Database.Orville.PostgreSQL.Connection
import qualified Database.Orville.PostgreSQL.Raw as ORaw

import Example.Data.Major
  ( Major(..)
  , MajorCollege(..)
  , MajorId(..)
  , MajorName(..)
  )
import Example.Data.Student (Student(..), StudentId(..), StudentName(..))
import Example.Schema.Student
  ( majorIdField
  , majorNameField
  , studentIdField
  , studentMajorField
  , studentNameField
  )
import Example.SchemaStudent (majorTable, studentSchema, studentTable)

main :: IO ()
main = do
  putStrLn "Connecting to db"
  connStr <- getEnv "TEST_CONN_STRING"
  putStrLn connStr
  poolConn <- createConnectionPool 1 10000 1 connStr
  let env = O.newOrvilleEnv poolConn
  _ <- O.runOrville initialInsertMajors env
  _ <- O.runOrville initialInsertStudents env
  catch
    ((O.runOrville insertStudentFail env) >>=
     T.putStrLn . studentNameText . studentName)
    (\e -> do
       let err = show (e :: SomeException)
       putStrLn
         ("Warning: Could not insert record. Postgres error message:\n\"" ++
          err ++ "\"\n"))
  resultSelect <- O.runOrville selectFirstTest env
  putStrLn "\nSelect first business major result:"
  case resultSelect of
    Just (student) -> T.putStrLn $ studentNameText $ studentName student
    Nothing -> putStrLn "no record returned"
  resultFind <- O.runOrville findRecordTest env
  putStrLn "\nFind record with ID 1:"
  case resultFind of
    Just (student) -> T.putStrLn $ studentNameText $ studentName student
    Nothing -> putStrLn "no record returned"
  resultSelectAll <- O.runOrville selectAllTest env
  putStrLn "\nSelect all result: "
  mapM_ (T.putStrLn . studentNameText . studentName) resultSelectAll
  deletedStudent <- O.runOrville deleteTest env
  putStrLn $
    "\nInserted and deleted: " ++
    unpack (studentNameText $ studentName deletedStudent) ++
    ", ID: " ++ show (studentIdInt $ studentId deletedStudent)
  deletedMajor <- O.runOrville deleteMajorSuccess env
  putStrLn $
    "\nInserted and deleted: " ++
    unpack (majorNameText $ majorName deletedMajor) ++
    ", ID: " ++ show (majorIdInt $ majorId deletedMajor)
  numDeletedMajors <- O.runOrville deleteWhereMajorSuccess env
  putStrLn $
    "\nNumber of records deleted from major table: " ++ (show numDeletedMajors)
  catch
    ((O.runOrville deleteMajorDoesNotExist env) >>=
     putStrLn . ("Number of records deleted from major table: " ++) . show)
    (\e -> do
       let err = show (e :: SomeException)
       putStrLn
         ("Warning: Could not delete record. Postgres error message:\n\"" ++
          err ++ "\"\n"))
  catch
    ((O.runOrville violateFKDelete env) >>= putStrLn . show)
    (\e -> do
       let err = show (e :: SomeException)
       putStrLn
         ("\nWarning: Could not delete record. Postgres error message:\n\"" ++
          err ++ "\"\n"))
  findRecordsResult <- O.runOrville findRecordsTest env
  let resultList = Map.toList findRecordsResult
  let names =
        map (\(_, student) -> studentNameText $ studentName student) resultList
  putStrLn "\nIDs 1-3:"
  mapM_ (T.putStrLn) names
  updateTest <- O.runOrville updateFieldsTest env
  putStr "\nTest update (number updated): "
  putStrLn $ show updateTest
  allEconStudents <- O.runOrville (findAllStudentsByMajor "Economics") env
  putStrLn "\nAll Econ Students"
  mapM_ (T.putStrLn . studentNameText . studentName) allEconStudents
  popOutput <- O.runOrville popRecordTest env
  putStrLn "\npopRecord' test:"
  putStrLn $ unpack (studentNameText $ studentName popOutput)
  popHasOutput <- O.runOrville popHasOneTest env
  putStrLn "\nhasOne' test:"
  putStrLn $ unpack (studentNameText $ studentName popHasOutput)
  hasManyTest <- O.runOrville popHasManyTest env
  putStrLn "\nhasMany test:"
  mapM_ (T.putStrLn . studentNameText . studentName) hasManyTest
  popManyTest <- O.runOrville popManyHasOne env
  putStrLn "\nhasOne' & popMany:"
  mapM_ (T.putStrLn . studentNameText . studentName) popManyTest
  popManyHasManyTest <- O.runOrville popManyHasMany env
  putStrLn "\nhasMany & popMany:"
  mapM_ (mapM_ (T.putStrLn . studentNameText . studentName)) popManyHasManyTest
  majorStudentsTuples <- O.runOrville runAllMajors env
  putStrLn "\nAll students in each major:"
  mapM_ printTuple majorStudentsTuples
  pure ()

printTuple :: StudentsForMajor -> IO ()
printTuple (major, studentList) = do
  putStrLn $ "\n" ++ show (majorNameText (majorName major)) ++ " students:"
  mapM_ (T.putStrLn . studentNameText . studentName) studentList

-- demonstrates insertRecord function
initialInsertMajors :: O.OrvilleT Postgres.Connection IO (Major MajorId)
initialInsertMajors = do
  resetToBlankSchema studentSchema
  _ <- O.insertRecord majorTable business
  _ <- O.insertRecord majorTable econ
  _ <- O.insertRecord majorTable math
  _ <- O.insertRecord majorTable chem
  O.insertRecord majorTable testMajor

-- demonstrates insertRecordMany function
initialInsertStudents :: O.OrvilleT Postgres.Connection IO ()
initialInsertStudents = do
  let student_list = [barry, allan, christine, erin, sam]
  O.insertRecordMany studentTable student_list

-- insert invalid (student has Major Id that is not in major table)
insertStudentFail :: O.OrvilleT Postgres.Connection IO (Student StudentId)
insertStudentFail = do
  O.insertRecord studentTable testStudent

selectFirstTest ::
     O.OrvilleT Postgres.Connection IO (Maybe (Student StudentId))
selectFirstTest = do
  let options = O.where_ $ (O..==) studentMajorField (MajorId 1)
  O.selectFirst studentTable options

selectAllTest :: O.OrvilleT Postgres.Connection IO [Student StudentId]
selectAllTest = do
  let options = O.SelectOptions mempty mempty mempty mempty mempty mempty
  O.selectAll studentTable options

findRecordTest :: O.OrvilleT Postgres.Connection IO (Maybe (Student StudentId))
findRecordTest = do
  O.findRecord studentTable (StudentId 1)

deleteTest :: O.OrvilleT Postgres.Connection IO (Student StudentId)
deleteTest = do
  insertedStudent <- O.insertRecord studentTable allan
  O.deleteRecord studentTable (studentId insertedStudent)
  pure insertedStudent

deleteMajorSuccess :: O.OrvilleT Postgres.Connection IO (Major MajorId)
deleteMajorSuccess = do
  insertedMajor <- O.insertRecord majorTable testMajor
  O.deleteRecord majorTable (majorId insertedMajor)
  pure insertedMajor

deleteWhereMajorSuccess :: O.OrvilleT Postgres.Connection IO (Integer)
deleteWhereMajorSuccess = do
  let condit = [(O..==) majorIdField (MajorId 5)]
  O.deleteWhere majorTable condit

--Will succeed even though major with Id 6 does not exist
deleteMajorDoesNotExist :: O.OrvilleT Postgres.Connection IO (Integer)
deleteMajorDoesNotExist = do
  let condit = [(O..==) majorIdField (MajorId 6)]
  O.deleteWhere majorTable condit

-- invalid delete (tries to delete major still referenced in student table)
violateFKDelete :: O.OrvilleT Postgres.Connection IO (Integer)
violateFKDelete = do
  let condit = [(O..==) majorIdField (MajorId 2)]
  O.deleteWhere majorTable condit

findRecordsTest ::
     O.OrvilleT Postgres.Connection IO (Map.Map StudentId (Student StudentId))
findRecordsTest = do
  let id_list = [StudentId 1, StudentId 2, StudentId 3]
  O.findRecords studentTable id_list

findRecordsByTest ::
     O.OrvilleT Postgres.Connection IO (Map.Map StudentId [Student StudentId])
findRecordsByTest = do
  let options = O.where_ $ (O..==) studentMajorField (MajorId 2)
  O.findRecordsBy studentTable studentIdField options

updateFieldsTest :: O.OrvilleT Postgres.Connection IO (Integer)
updateFieldsTest = do
  let updates = [O.fieldUpdate studentMajorField (MajorId 4)]
  let condit = [(O..==) studentNameField (StudentName $ pack "Erin Valentino")]
  O.updateFields studentTable updates condit

findMajor ::
     String -> O.OrvilleT Postgres.Connection IO (Maybe (Major MajorId))
findMajor str = do
  let options = O.where_ $ (O..==) majorNameField (MajorName $ pack str)
  O.selectFirst majorTable options

findStudentsByMajorId ::
     MajorId -> O.OrvilleT Postgres.Connection IO [Student StudentId]
findStudentsByMajorId majId = do
  let options = O.where_ $ (O..==) studentMajorField majId
  O.selectAll studentTable options

findAllStudentsByMajor ::
     String -> O.OrvilleT Postgres.Connection IO [Student StudentId]
findAllStudentsByMajor majorStr = do
  maybeMajor <- findMajor majorStr
  case maybeMajor of
    Nothing -> pure []
    Just major -> do
      findStudentsByMajorId (majorId major)

popRecordTest :: O.OrvilleT Postgres.Connection IO (Student StudentId)
popRecordTest = do
  let poppedRecord = O.popRecord' studentTable (StudentId 1)
  O.popThrow poppedRecord ()

popHasOneTest :: O.OrvilleT Postgres.Connection IO (Student StudentId)
popHasOneTest = do
  let hasOneRec = O.hasOne' studentTable studentMajorField
  O.popThrow hasOneRec (MajorId 3)

popHasManyTest :: O.OrvilleT Postgres.Connection IO [Student StudentId]
popHasManyTest = do
  let manyRecs = O.hasMany studentTable studentMajorField
  O.popThrow manyRecs (MajorId 3)

popManyHasOne :: O.OrvilleT Postgres.Connection IO [Student StudentId]
popManyHasOne = do
  let result = O.hasOne' studentTable studentMajorField
  let manyResult = O.popMany result
  O.popThrow manyResult [MajorId 1, MajorId 2, MajorId 3]

popManyHasMany :: O.OrvilleT Postgres.Connection IO [[Student StudentId]]
popManyHasMany = do
  let hasManyResult = O.hasMany studentTable studentMajorField
  let popManyResult = O.popMany hasManyResult
  O.popThrow popManyResult [MajorId 1, MajorId 2, MajorId 3]

type StudentsForMajor = (Major MajorId, [Student StudentId])

runAllMajors :: O.OrvilleT Postgres.Connection IO [StudentsForMajor]
runAllMajors = O.popThrow (allMajors >>> studentsForMajors) ()

allMajors :: O.Popper a [Major MajorId]
allMajors = O.popTable majorTable mempty

studentsForMajors :: O.Popper [Major MajorId] [StudentsForMajor]
studentsForMajors = O.popMany $ studentsWithMajor

studentsWithMajor :: O.Popper (Major MajorId) StudentsForMajor
studentsWithMajor = O.kern &&& getStudentsByMajor

getStudentsByMajor :: O.Popper (Major MajorId) [Student StudentId]
getStudentsByMajor = majorIdPopper >>> getStudentsByMajorId

getStudentsByMajorId :: O.Popper (MajorId) [Student StudentId]
getStudentsByMajorId = O.hasMany studentTable studentMajorField

majorIdPopper :: O.Popper (Major MajorId) (MajorId)
majorIdPopper = O.fromKern majorId

resetToBlankSchema :: O.MonadOrville conn m => O.SchemaDefinition -> m ()
resetToBlankSchema schemaDef = do
  results <- ORaw.selectSqlRows "SELECT current_user" []
  case results of
    [[("current_user", currentUser)]] ->
      void $ ORaw.updateSql ("DROP OWNED BY " ++ convert currentUser) []
    _ ->
      error $ "Expected single 'current_user' result row, got " ++ show results
  O.migrateSchema schemaDef

business :: Major ()
business =
  Major
    { majorId = ()
    , majorName = MajorName $ pack "Business"
    , majorCollege = LiberalArts
    }

econ :: Major ()
econ =
  Major
    { majorId = ()
    , majorName = MajorName $ pack "Economics"
    , majorCollege = LiberalArts
    }

math :: Major ()
math =
  Major
    { majorId = ()
    , majorName = MajorName $ pack "Math"
    , majorCollege = NaturalScience
    }

chem :: Major ()
chem =
  Major
    { majorId = ()
    , majorName = MajorName $ pack "Chemistry"
    , majorCollege = NaturalScience
    }

testMajor :: Major ()
testMajor =
  Major
    { majorId = ()
    , majorName = MajorName $ pack "Test Major"
    , majorCollege = NaturalScience
    }

allan :: Student ()
allan =
  Student
    { studentId = ()
    , studentName = StudentName $ pack "Allan Sherwood"
    , studentMajor = MajorId 1
    }

barry :: Student ()
barry =
  Student
    { studentId = ()
    , studentName = StudentName $ pack "Barry Zimmer"
    , studentMajor = MajorId 2
    }

christine :: Student ()
christine =
  Student
    { studentId = ()
    , studentName = StudentName $ pack "Christine Brown"
    , studentMajor = MajorId 3
    }

erin :: Student ()
erin =
  Student
    { studentId = ()
    , studentName = StudentName $ pack "Erin Valentino"
    , studentMajor = MajorId 2
    }

sam :: Student ()
sam =
  Student
    { studentId = ()
    , studentName = StudentName $ pack "Samuel Frazier"
    , studentMajor = MajorId 3
    }

testStudent :: Student ()
testStudent =
  Student
    { studentId = ()
    , studentName = StudentName $ pack "Test Student"
    , studentMajor = MajorId 6
    }

erinNew :: Student ()
erinNew =
  Student
    { studentId = ()
    , studentName = StudentName $ pack "Erin K. Valentino"
    , studentMajor = MajorId 1
    }
