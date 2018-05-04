{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad (void)
import Control.Exception
import Data.Convertible (convert)
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Data.Map.Strict as Map
import System.Environment (getEnv)
import qualified Data.Text.IO as T
import Data.Text (Text, pack, unpack)

import qualified Database.Orville as O
import qualified Database.Orville.Raw as ORaw
import Database.Orville.PostgresSQL

import Example.Data.Major ( Major(..), MajorId(..), MajorName(..), MajorCollege(..))
import Example.Data.Student ( Student(..), StudentId(..), StudentName(..) )
import Example.Schema.Student ( studentIdField, studentNameField, studentMajorField, majorIdField, majorNameField, majorCollegeField )
import Example.SchemaStudent ( studentSchema, studentTable, majorTable )


main :: IO ()
main = do
  putStrLn "Connecting to db"
  connStr <- getEnv "TEST_CONN_STRING"
  putStrLn connStr
  poolConn <- createConnectionPool 1 10000 1 connStr
  let env = O.newOrvilleEnv poolConn
  _ <- O.runOrville initialInsertMajors env
  _ <- O.runOrville initialInsertStudents env
  
  catch ((O.runOrville insertStudentFail env) >>= T.putStrLn . studentNameText . studentName) 
        (\e -> do let err = show (e :: SomeException)
                  putStrLn ("Warning: Could not insert record. Postgres error message:\n\"" ++ err ++ "\"\n"))

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
  mapM_ (T.putStrLn . studentNameText. studentName) resultSelectAll
 
  deletedStudent <- O.runOrville deleteTest env
  putStrLn $ "\nInserted and deleted: " ++ unpack (studentNameText $ studentName deletedStudent) ++ ", ID: " ++ show (studentIdInt $ studentId deletedStudent)

  deletedMajor <- O.runOrville deleteMajorSuccess env
  putStrLn $ "\nInserted and deleted: " ++ unpack (majorNameText $ majorName deletedMajor) ++ ", ID: " ++ show (majorIdInt $ majorId deletedMajor)

  numDeletedMajors <- O.runOrville deleteWhereMajorSuccess env
  putStrLn $ "\nNumber of records deleted from major table: " ++ (show numDeletedMajors)

  catch ((O.runOrville deleteMajorDoesNotExist env) >>= putStrLn . ("Number of records deleted from major table: " ++) . show) 
      (\e -> do let err = show (e :: SomeException)
                putStrLn ("Warning: Could not delete record. Postgres error message:\n\"" ++ err ++ "\"\n"))

  catch ((O.runOrville violateFKDelete env) >>= putStrLn . show) 
    (\e -> do let err = show (e :: SomeException)
              putStrLn ("\nWarning: Could not delete record. Postgres error message:\n\"" ++ err ++ "\"\n"))

  findRecordsResult <- O.runOrville findRecordsTest env
  let resultList = Map.toList findRecordsResult
  let names = map (\(id, student) -> studentNameText $ studentName student) resultList
  putStrLn "\nIDs 1-3:"
  mapM_ (T.putStrLn) names

  updateTest <- O.runOrville updateFieldsTest env
  putStr "\nTest update (number updated): "
  putStrLn $ show updateTest

  allEconStudents <- O.runOrville (findAllStudentsByMajor "Economics") env
  putStrLn "\nAll Econ Students"
  mapM_ (T.putStrLn . studentNameText . studentName) allEconStudents

  pure ()

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
  let student_list = [barry, allan, christine, erin]
  O.insertRecordMany studentTable student_list

-- insert invalid (student has Major Id that is not in major table)
insertStudentFail :: O.OrvilleT Postgres.Connection IO (Student StudentId)
insertStudentFail = do
  O.insertRecord studentTable testStudent

selectFirstTest :: O.OrvilleT Postgres.Connection IO (Maybe (Student StudentId))
selectFirstTest = do
  let options = O.where_ $ (O..==) studentMajorField (MajorId 1)
  O.selectFirst studentTable options

selectAllTest :: O.OrvilleT Postgres.Connection IO [Student StudentId]
selectAllTest = do
  let options = O.SelectOptions mempty mempty mempty mempty mempty
  O.selectAll studentTable options

findRecordTest :: O.OrvilleT Postgres.Connection IO (Maybe (Student StudentId))
findRecordTest = do
  O.findRecord studentTable (StudentId 1)

deleteTest :: O.OrvilleT Postgres.Connection IO (Student StudentId)
deleteTest = do
  insertedStudent <- O.insertRecord studentTable allan
  O.deleteRecord studentTable insertedStudent
  pure insertedStudent

deleteMajorSuccess :: O.OrvilleT Postgres.Connection IO (Major MajorId)
deleteMajorSuccess = do
  insertedMajor <- O.insertRecord majorTable testMajor
  O.deleteRecord majorTable insertedMajor
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

findRecordsTest :: O.OrvilleT Postgres.Connection IO (Map.Map StudentId (Student StudentId))
findRecordsTest = do
  let id_list = [StudentId 1, StudentId 2, StudentId 3]
  O.findRecords studentTable id_list

findRecordsByTest :: O.OrvilleT Postgres.Connection IO (Map.Map StudentId [Student StudentId])
findRecordsByTest = do
  let options = O.where_ $ (O..==) studentMajorField (MajorId 2)
  O.findRecordsBy studentTable studentIdField options 

updateFieldsTest :: O.OrvilleT Postgres.Connection IO (Integer)
updateFieldsTest = do
  let updates = [O.fieldUpdate studentMajorField (MajorId 4)] 
  let condit = [(O..==) studentNameField (StudentName $ pack "Erin Valentino")]
  O.updateFields studentTable updates condit

findMajor :: String -> O.OrvilleT Postgres.Connection IO (Maybe (Major MajorId))
findMajor str = do
  let options = O.where_ $ (O..==) majorNameField (MajorName $ pack str)
  O.selectFirst majorTable options

findStudentsByMajorId :: MajorId -> O.OrvilleT Postgres.Connection IO [Student StudentId]
findStudentsByMajorId majId = do
  let options = O.where_ $ (O..==) studentMajorField majId
  O.selectAll studentTable options

findAllStudentsByMajor :: String -> O.OrvilleT Postgres.Connection IO [Student StudentId]
findAllStudentsByMajor majorStr = do
  maybeMajor <- findMajor majorStr
  case maybeMajor of
    Nothing -> pure []
    Just major -> do
      findStudentsByMajorId (majorId major)


resetToBlankSchema :: O.SchemaDefinition -> O.Orville ()
resetToBlankSchema schemaDef = do
  results <- ORaw.selectSqlRows "SELECT current_user" []
  case results of
    [[("current_user", currentUser)]]
     -> void $ ORaw.updateSql ("DROP OWNED BY " ++ convert currentUser) []
    _ ->
      error $ "Expected single 'current_user' result row, got " ++ show results
  O.migrateSchema schemaDef
  

business :: Major ()
business = Major {majorId = (), majorName = MajorName $ pack "Business", majorCollege = LiberalArts}

econ :: Major ()
econ = Major {majorId = (), majorName = MajorName $ pack "Economics", majorCollege = LiberalArts}

math :: Major ()
math = Major {majorId = (), majorName = MajorName $ pack "Math", majorCollege = NaturalScience}

chem :: Major ()
chem = Major {majorId = (), majorName = MajorName $ pack "Chemistry", majorCollege = NaturalScience}

testMajor :: Major ()
testMajor = Major {majorId = (), majorName = MajorName $ pack "Test Major", majorCollege = NaturalScience}

allan :: Student ()
allan =
  Student {studentId = (), studentName = StudentName $ pack "Allan Sherwood", studentMajor = MajorId 1}

barry :: Student ()
barry =
  Student {studentId = (), studentName = StudentName $ pack "Barry Zimmer", studentMajor = MajorId 2}

christine :: Student ()
christine =
  Student {studentId = (), studentName = StudentName $ pack "Christine Brown", studentMajor = MajorId 3}

erin :: Student ()
erin =
  Student {studentId = (), studentName = StudentName $ pack "Erin Valentino", studentMajor = MajorId 2}

testStudent :: Student ()
testStudent =
  Student {studentId = (), studentName = StudentName $ pack "Test Student", studentMajor = MajorId 6}

erinNew :: Student ()
erinNew =
  Student {studentId = (), studentName = StudentName $ pack "Erin K. Valentino", studentMajor = MajorId 1}
