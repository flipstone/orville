{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (void)
import Data.Convertible (convert)
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Data.Map.Strict as Map

import qualified Database.Orville as O
import qualified Database.Orville.Raw as ORaw
import Database.Orville.PostgresSQL

import Example.Data.Major (Major(..), MajorId(..), MajorName(..), MajorCollege(..), College(..))
import Example.Data.Student ( Student(..), StudentId(..), StudentName(..), StudentMajor(..))
import Example.Schema.Student ( studentIdField, studentNameField, studentMajorField )
import Example.SchemaStudent (studentSchema, studentTable)


main :: IO ()
main = do
  putStrLn "Connecting to db"
  -- To do: reference environment variable
  let connStr = "host=testdb user=orville_test"
  putStrLn connStr
  poolConn <- createConnectionPool 1 10000 1 connStr
  let env = O.newOrvilleEnv poolConn
  _ <- O.runOrville initialInsert env
  _ <- O.runOrville insertManyTest env

  resultSelect <- O.runOrville selectFirstTest env
  putStrLn "\nSelect first business major result:"
  -- To do: eliminate case statement
  case resultSelect of 
    Just (student) -> putStrLn $ getStudentName $ studentName student
    Nothing -> putStrLn "no record returned"

  resultFind <- O.runOrville findRecordTest env
  putStrLn "\nFind record with ID 1:"
  case resultFind of
    Just (student) -> putStrLn $ getStudentName $ studentName student
    Nothing -> putStrLn "no record returned"
  
  resultSelectAll <- O.runOrville selectAllTest env
  putStrLn "\nSelect all result: "
  mapM_ (putStrLn . getStudentName . studentName) resultSelectAll
 
  deletedStudent <- O.runOrville deleteTest env
  putStrLn $ "\nInserted and deleted: " ++ (getStudentName $ studentName deletedStudent) ++ ", ID: " ++ (getStudentId $ studentId deletedStudent)

  findRecordsResult <- O.runOrville findRecordsTest env
  let resultList = Map.toList findRecordsResult
  let names = map (\(id, student) -> getStudentName $ studentName student) resultList
  putStrLn "\nIDs 1-3:"
  mapM_ (putStrLn) names

  updateTest <- O.runOrville updateFieldsTest env
  putStr "\nTest update (number updated): "
  putStrLn $ show updateTest


  pure ()
 


initialInsert :: O.OrvilleT Postgres.Connection IO (Student StudentId)
initialInsert = do
  resetToBlankSchema studentSchema
  _ <- O.insertRecord studentTable barry
  _ <- O.insertRecord studentTable allan
  _ <- O.insertRecord studentTable christine
  O.insertRecord studentTable erin

selectFirstTest :: O.OrvilleT Postgres.Connection IO (Maybe (Student StudentId))
selectFirstTest = do
  let condit = [(O..==) studentMajorField (StudentMajor "Business")]
  let options = O.SelectOptions condit mempty mempty mempty mempty
  O.selectFirst studentTable options

selectAllTest :: O.OrvilleT Postgres.Connection IO [Student StudentId]
selectAllTest = do
  let options = O.SelectOptions mempty mempty mempty mempty mempty
  O.selectAll studentTable options

findRecordTest :: O.OrvilleT Postgres.Connection IO (Maybe (Student StudentId))
findRecordTest = do
  O.findRecord studentTable (StudentId "1")

deleteTest :: O.OrvilleT Postgres.Connection IO (Student StudentId)
deleteTest = do
  insertedStudent <- O.insertRecord studentTable allan
  O.deleteRecord studentTable insertedStudent
  pure insertedStudent

insertManyTest :: O.OrvilleT Postgres.Connection IO ()
insertManyTest = do
  let liszt = [allan, barry]
  O.insertRecordMany studentTable liszt

findRecordsTest :: O.OrvilleT Postgres.Connection IO (Map.Map StudentId (Student StudentId))
findRecordsTest = do
  let liszt = [StudentId "1", StudentId "2", StudentId "3"]
  O.findRecords studentTable liszt

findRecordsByTest :: O.OrvilleT Postgres.Connection IO (Map.Map StudentId [Student StudentId])
findRecordsByTest = do
  let condit = [(O..==) studentMajorField (StudentMajor "Economics")]
  let options = O.SelectOptions condit mempty mempty mempty mempty
  O.findRecordsBy studentTable studentIdField options 

updateFieldsTest :: O.OrvilleT Postgres.Connection IO (Integer)
updateFieldsTest = do
  let updates = [O.fieldUpdate studentMajorField (StudentMajor "Econ")] 
  let condit = [(O..==) studentMajorField (StudentMajor "Economics")]
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
  

allan :: Student ()
allan =
  Student {studentId = (), studentName = StudentName "Allan Sherwood", studentMajor = StudentMajor "Business"}

barry :: Student ()
barry =
  Student {studentId = (), studentName = StudentName "Barry Zimmer", studentMajor = StudentMajor "Economics"}

christine :: Student ()
christine =
  Student {studentId = (), studentName = StudentName "Christine Brown", studentMajor = StudentMajor "Math"}

erin :: Student ()
erin =
  Student {studentId = (), studentName = StudentName "Erin Valentino", studentMajor = StudentMajor "Economics"}

erinNew :: Student ()
erinNew =
  Student {studentId = (), studentName = StudentName "Erin K. Valentino", studentMajor = StudentMajor "Government"}
