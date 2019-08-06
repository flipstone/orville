module Database.Orville.Oracle.Internal.SchemaState
  ( SchemaState
  , schemaStateTableColumns
  , schemaStateTableExists
  , schemaStateIndexExists
  , schemaStateConstraintExists
  , loadSchemaState
  ) where

import Control.Monad (forM, void)
import Data.Convertible (convert)
import qualified Database.HDBC as HDBC

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type TableName = String

type IndexName = String

type ConstraintName = String

type Columns = [(String, HDBC.SqlColDesc)]

data SchemaState = SchemaState
  { schemaStateTables :: Map.Map TableName Columns
  , schemaStateIndexes :: Set.Set IndexName
  , schemaStateConstraints :: Set.Set ConstraintName
  }

schemaStateTableColumns :: TableName -> SchemaState -> Maybe Columns
schemaStateTableColumns name = Map.lookup name . schemaStateTables

schemaStateTableExists :: TableName -> SchemaState -> Bool
schemaStateTableExists name = Map.member name . schemaStateTables

schemaStateIndexExists :: IndexName -> SchemaState -> Bool
schemaStateIndexExists name = Set.member name . schemaStateIndexes

schemaStateConstraintExists :: ConstraintName -> SchemaState -> Bool
schemaStateConstraintExists name = Set.member name . schemaStateConstraints

loadSchemaState :: HDBC.IConnection conn => conn -> IO SchemaState
loadSchemaState conn = do
  SchemaState <$> getTables conn <*> getIndexes conn <*> getConstraints conn

getTables :: HDBC.IConnection conn => conn -> IO (Map.Map TableName Columns)
getTables conn = do
  tables <- HDBC.getTables conn
  fmap Map.fromList $
    forM tables $ \table -> do
      columns <- HDBC.describeTable conn table
      pure (table, columns)

getIndexes :: HDBC.IConnection conn => conn -> IO (Set.Set IndexName)
getIndexes conn = do
  query <-
    HDBC.prepare
      conn
      "SELECT indexname FROM pg_indexes WHERE schemaname = current_schema();"
  void $ HDBC.execute query []
  Set.fromList <$> map (convert . head) <$> HDBC.fetchAllRows' query

getConstraints :: HDBC.IConnection conn => conn -> IO (Set.Set ConstraintName)
getConstraints conn = do
  query <-
    HDBC.prepare
      conn
      "SELECT conname \
                        \FROM pg_constraint \
                        \JOIN pg_namespace ON pg_namespace.oid = pg_constraint.connamespace \
                        \WHERE nspname = current_schema()"
  void $ HDBC.execute query []
  Set.fromList <$> map (convert . head) <$> HDBC.fetchAllRows' query
