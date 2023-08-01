module Orville.PostgreSQL.Schema.TableIdentifier
  ( TableIdentifier
  , unqualifiedNameToTableId
  , setTableIdSchema
  , tableIdQualifiedName
  , tableIdUnqualifiedName
  , tableIdSchemaName
  , tableIdToString
  , tableIdUnqualifiedNameString
  , tableIdSchemaNameString
  )
where

import qualified Orville.PostgreSQL.Expr as Expr

{- |
  An identifier used by Orville to identify a particular table in a particular
  schema.
-}
data TableIdentifier = TableIdentifier
  { i_tableIdName :: String
  , i_tableIdSchema :: Maybe String
  }
  deriving (Eq, Ord, Show)

{- |
  Constructs a 'TableIdentifier' where the table's name will not be qualified
  by a particular schema.
-}
unqualifiedNameToTableId :: String -> TableIdentifier
unqualifiedNameToTableId name =
  TableIdentifier
    { i_tableIdName = name
    , i_tableIdSchema = Nothing
    }

{- |
  Sets the schema of the 'TableIdentifier'. Wherever applicable, references to
  the table will be qualified by the given scheme name.
-}
setTableIdSchema :: String -> TableIdentifier -> TableIdentifier
setTableIdSchema schema tableId =
  tableId
    { i_tableIdSchema = Just schema
    }

{- |
  Returns the 'Expr.Qualified Expr.TableName' that should be used to refer to the
  table in SQL queries.
-}
tableIdQualifiedName :: TableIdentifier -> Expr.Qualified Expr.TableName
tableIdQualifiedName tableId =
  Expr.qualifyTable
    (tableIdSchemaName tableId)
    (tableIdUnqualifiedName tableId)

{- |
  Returns the unqualified 'Expr.TableName' that should be used to refer to the
  table in SQL queries where an unqualified reference is appropriate.
-}
tableIdUnqualifiedName :: TableIdentifier -> Expr.TableName
tableIdUnqualifiedName =
  Expr.tableName . i_tableIdName

{- |
  Returns the 'Expr.SchemaName' (if any) that should be used to qualify
  references to the table in SQL queries.
-}
tableIdSchemaName :: TableIdentifier -> Maybe Expr.SchemaName
tableIdSchemaName =
  fmap Expr.schemaName . i_tableIdSchema

{- |
  Retrieves the unqualified name of the table as a string.
-}
tableIdUnqualifiedNameString :: TableIdentifier -> String
tableIdUnqualifiedNameString =
  i_tableIdName

{- |
  Retrieves the schema name of the table as a string
-}
tableIdSchemaNameString :: TableIdentifier -> Maybe String
tableIdSchemaNameString =
  i_tableIdSchema

{- |
  Converts a 'TableIdentifier' for a string for descriptive purposes. The
  name will be qualified if a schema name has been set for the identifier.

  Note: You should not use this function for building SQL expressions. Use
  'tableIdQualifiedName' instead for that.
-}
tableIdToString :: TableIdentifier -> String
tableIdToString tableId =
  case i_tableIdSchema tableId of
    Nothing ->
      i_tableIdName tableId
    Just schema ->
      schema <> "." <> i_tableIdName tableId
