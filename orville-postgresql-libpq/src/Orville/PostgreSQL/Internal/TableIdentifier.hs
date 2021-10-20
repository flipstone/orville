module Orville.PostgreSQL.Internal.TableIdentifier
  ( TableIdentifier,
    unqualifiedNameToTableId,
    setTableIdSchema,
    tableIdQualifiedName,
    tableIdUnqualifiedName,
    tableIdSchemaName,
    tableIdToString,
    tableIdUnqualifiedNameString,
    tableIdSchemaNameString,
  )
where

import qualified Orville.PostgreSQL.Internal.Expr as Expr

{- |
  An identifier used by Orville to identify a particular table in a particular
  schema.
-}
data TableIdentifier = TableIdentifier
  { _tableIdName :: String
  , _tableIdSchema :: Maybe String
  }
  deriving (Eq, Ord, Show)

{- |
  Constructs a 'TableIdentifier' where the table's name will not be qualified
  by a particular schema.
-}
unqualifiedNameToTableId :: String -> TableIdentifier
unqualifiedNameToTableId name =
  TableIdentifier
    { _tableIdName = name
    , _tableIdSchema = Nothing
    }

{- |
  Sets the schema of the 'TableIdentifier'. Wherever applicable, references to
  the table will be qualified by the given scheme name.
-}
setTableIdSchema :: String -> TableIdentifier -> TableIdentifier
setTableIdSchema schema tableId =
  tableId
    { _tableIdSchema = Just schema
    }

{- |
  Returns the 'Expr.QualifiedTableName' that should be used to refer to the
  table in SQL queries.
-}
tableIdQualifiedName :: TableIdentifier -> Expr.QualifiedTableName
tableIdQualifiedName tableId =
  Expr.qualifiedTableName
    (tableIdSchemaName tableId)
    (tableIdUnqualifiedName tableId)

{- |
  Returns the unqualified 'Expr.TableName' that should be used to refer to the
  table in SQL queries where an unqualified reference is appropriate.
-}
tableIdUnqualifiedName :: TableIdentifier -> Expr.TableName
tableIdUnqualifiedName =
  Expr.tableName . _tableIdName

{- |
  Returns the 'Expr.SchemaName' (if any) that should be used to qualify
  references to the table in SQL queries.
-}
tableIdSchemaName :: TableIdentifier -> Maybe Expr.SchemaName
tableIdSchemaName =
  fmap Expr.schemaName . _tableIdSchema

{- |
  Retrieves the unqualified name of the table as a string.
-}
tableIdUnqualifiedNameString :: TableIdentifier -> String
tableIdUnqualifiedNameString =
  _tableIdName

{- |
  Retrieves the schema name of the table as a string
-}
tableIdSchemaNameString :: TableIdentifier -> Maybe String
tableIdSchemaNameString =
  _tableIdSchema

{- |
  Converts a 'TableIdentifier' for a string for descriptive purposes. The
  name will be qualified if a schema name has been set for the identifier.

  Note: You should not use this function for building SQL expressions. Use
  'tableIdQualifiedName' instead for that.
-}
tableIdToString :: TableIdentifier -> String
tableIdToString tableId =
  case _tableIdSchema tableId of
    Nothing ->
      _tableIdName tableId
    Just schema ->
      schema <> "." <> _tableIdName tableId
