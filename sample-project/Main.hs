{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (void)
import Data.Convertible (convert)
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Data.Map.Strict as Map
import System.Environment (getEnv)

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
  _ <- O.runOrville initialInsertStudents env
  _ <- O.runOrville initialInsertMajors env

  resultSelect <- O.runOrville selectFirstTest env
  putStrLn "\nSelect first business major result:"
  -- To do: eliminate case statement
  case resultSelect of 
    Just (student) -> putStrLn $ studentNameString $ studentName student
    Nothing -> putStrLn "no record returned"

  resultFind <- O.runOrville findRecordTest env
  putStrLn "\nFind record with ID 1:"
  case resultFind of
    Just (student) -> putStrLn $ studentNameString $ studentName student
    Nothing -> putStrLn "no record returned"
  
  resultSelectAll <- O.runOrville selectAllTest env
  putStrLn "\nSelect all result: "
  mapM_ (putStrLn . studentNameString . studentName) resultSelectAll
 
  deletedStudent <- O.runOrville deleteTest env
  putStrLn $ "\nInserted and deleted: " ++ (studentNameString $ studentName deletedStudent) ++ ", ID: " ++ show (studentIdInt $ studentId deletedStudent)

  findRecordsResult <- O.runOrville findRecordsTest env
  let resultList = Map.toList findRecordsResult
  let names = map (\(id, student) -> studentNameString $ studentName student) resultList
  putStrLn "\nIDs 1-3:"
  mapM_ (putStrLn) names

  updateTest <- O.runOrville updateFieldsTest env
  putStr "\nTest update (number updated): "
  putStrLn $ show updateTest


  pure ()

-- demonstrates insertRecordMary function
initialInsertStudents :: O.OrvilleT Postgres.Connection IO ()
initialInsertStudents = do
  resetToBlankSchema studentSchema
  let student_list = [barry, allan, christine, erin]
  O.insertRecordMany studentTable student_list

-- demonstrates insertRecord function
initialInsertMajors :: O.OrvilleT Postgres.Connection IO (Major MajorId)
initialInsertMajors = do
  _ <- O.insertRecord majorTable business
  _ <- O.insertRecord majorTable econ
  _ <- O.insertRecord majorTable math
  O.insertRecord majorTable chem

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
  let condit = [(O..==) studentNameField (StudentName "Erin Valentino")]
  O.updateFields studentTable updates condit


resetToBlankSchema :: O.SchemaDefinition -> O.Orville ()
resetToBlankSchema schemaDef = do
  results <- ORaw.selectSqlRows "SELECT current_user" []
  case results of
    [[("current_user", currentUser)]]
     -> void $ ORaw.updateSql ("DROP OWNED BY " ++ convert currentUser) []
    _ ->
      error $ "Expected single 'current_user' result row, got " ++ show results
  O.migrateSchema schemaDef
  

business :: Major()
business = Major {majorId = (), majorName = MajorName "Business", majorCollege = LiberalArts}

econ :: Major()
econ = Major {majorId = (), majorName = MajorName "Economics", majorCollege = LiberalArts}

math :: Major()
math = Major {majorId = (), majorName = MajorName "Math", majorCollege = NaturalScience}

chem :: Major()
chem = Major {majorId = (), majorName = MajorName "Chemistry", majorCollege = NaturalScience}

allan :: Student ()
allan =
  Student {studentId = (), studentName = StudentName "Allan Sherwood", studentMajor = MajorId 1}

barry :: Student ()
barry =
  Student {studentId = (), studentName = StudentName "Barry Zimmer", studentMajor = MajorId 2}

christine :: Student ()
christine =
  Student {studentId = (), studentName = StudentName "Christine Brown", studentMajor = MajorId 3}

erin :: Student ()
erin =
  Student {studentId = (), studentName = StudentName "Erin Valentino", studentMajor = MajorId 2}

erinNew :: Student ()
erinNew =
  Student {studentId = (), studentName = StudentName "Erin K. Valentino", studentMajor = MajorId 1}
