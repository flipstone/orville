{-|
Module    : Database.Orville.Internal.MigrateConstraint
Copyright : Fliptsone Technology Partners 2016-2018
License   : MIT
-}

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

import            Database.Orville.Internal.Execute
import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.Types

createConstraint :: MonadOrville conn m => conn -> ConstraintDefinition -> m ()
createConstraint conn (ConstraintDefinition {..}) = do
  let ddl = intercalate " " [ "ALTER TABLE"
                            , "\"" ++ constraintTable ++ "\""
                            , "ADD CONSTRAINT"
                            , "\"" ++ constraintName ++ "\""
                            , constraintBody
                            ]

  executingSql DDLQuery ddl $ void $ run conn ddl []

dropConstraint :: MonadOrville conn m => conn -> String -> String -> m ()
dropConstraint conn tableName constraintName = do
  let ddl = "ALTER TABLE " ++ tableName ++ " DROP CONSTRAINT " ++ constraintName
  executingSql DDLQuery ddl $ void $ run conn ddl []

getConstraints :: IConnection conn => conn -> IO [String]
getConstraints conn = do
  query <- prepare conn "SELECT conname \
                        \FROM pg_constraint \
                        \JOIN pg_namespace ON pg_namespace.oid = pg_constraint.connamespace \
                        \WHERE nspname = 'public'"

  void $ execute query []
  map (convert . head) <$> fetchAllRows' query
