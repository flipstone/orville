{-# LANGUAGE RecordWildCards #-}
module Database.Orville.Internal.MigrateConstraint
  ( createConstraint
  , dropConstraint
  , getConstraints
  ) where

import            Control.Monad
import            Data.Convertible
import            Data.List
import            Database.HDBC

import            Database.Orville.Internal.Types

createConstraint :: IConnection conn => conn -> ConstraintDefinition -> IO ()
createConstraint conn (ConstraintDefinition {..}) = do
  let ddl = intercalate " " [ "ALTER TABLE"
                            , "\"" ++ constraintTable ++ "\"" 
                            , "ADD CONSTRAINT"
                            , "\"" ++ constraintName ++ "\"" 
                            , constraintBody
                            ]

  putStrLn ddl
  void $ run conn ddl []

dropConstraint :: IConnection conn => conn -> String -> String -> IO ()
dropConstraint conn tableName constraintName = do
  let ddl = "ALTER TABLE " ++ tableName ++ " DROP CONSTRAINT " ++ constraintName
  putStrLn ddl
  void $ run conn ddl []

getConstraints :: IConnection conn => conn -> IO [String]
getConstraints conn = do
  query <- prepare conn "SELECT conname \
                        \FROM pg_constraint \
                        \JOIN pg_namespace ON pg_namespace.oid = pg_constraint.connamespace \
                        \WHERE nspname = 'public'"

  void $ execute query []
  map (convert . head) <$> fetchAllRows' query

