module Orville.PostgreSQL.Internal.ConstraintDefinition
  ( ConstraintDefinition,
    uniqueConstraint,
    foreignKeyConstraint,
    ForeignReference (localFieldName, foreignFieldName),
    foreignReference,
    ConstraintMigrationKey (ConstraintMigrationKey, constraintKeyType, constraintKeyColumns, constraintKeyForeignTable, constraintKeyForeignColumns),
    ConstraintKeyType (UniqueConstraint, ForeignKeyConstraint),
    constraintMigrationKey,
    constraintSqlExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Internal.TableIdentifier as TableIdentifier

{- |
  Defines a constraint that can be added to a
  'Orville.PostgreSQL.TableDefinition'. Use one of the constructor functions
  below (such as 'uniqueConstraint') to construct the constraint definition you
  wish to have and then use 'Orville.PostgreSQL.addTableConstraints'. to add
  them to your table definition. Orville will then add the constraint next time
  you run auto-migrations.
-}
data ConstraintDefinition = ConstraintDefinition
  { _constraintSqlExpr :: Expr.TableConstraint
  , _constraintMigrationKey :: ConstraintMigrationKey
  }

{- |
  The key used by Orville to determine whether a constraint should be added to
  a table when performing auto migrations. For most use cases the constructor
  functions that build a 'ConstraintDefinition' will create this automatically
  for you.
-}
data ConstraintMigrationKey = ConstraintMigrationKey
  { constraintKeyType :: ConstraintKeyType
  , constraintKeyColumns :: Maybe [FieldDefinition.FieldName]
  , constraintKeyForeignTable :: Maybe TableIdentifier.TableIdentifier
  , constraintKeyForeignColumns :: Maybe [FieldDefinition.FieldName]
  }
  deriving (Eq, Ord, Show)

{- |
  The kind of constraint that is described by a 'ConstraintMigrationKey' (e.g. unique, foreign key).
-}
data ConstraintKeyType
  = UniqueConstraint
  | ForeignKeyConstraint
  deriving (Eq, Ord, Show)

{- |
  Gets the 'ConstraintMigrationKey' for the 'ConstraintDefinition'
-}
constraintMigrationKey :: ConstraintDefinition -> ConstraintMigrationKey
constraintMigrationKey = _constraintMigrationKey

{- |
  Gets the SQL expression that will be used to add the constraint to the table.
-}
constraintSqlExpr :: ConstraintDefinition -> Expr.TableConstraint
constraintSqlExpr = _constraintSqlExpr

{- |
  Constructs a 'ConstraintDefinition' for a @UNIQUE@ constraint on the given
  columns.
-}
uniqueConstraint :: NonEmpty FieldDefinition.FieldName -> ConstraintDefinition
uniqueConstraint fieldNames =
  let expr =
        Expr.uniqueConstraint . fmap FieldDefinition.fieldNameToColumnName $ fieldNames

      migrationKey =
        ConstraintMigrationKey
          { constraintKeyType = UniqueConstraint
          , constraintKeyColumns = Just (NEL.toList fieldNames)
          , constraintKeyForeignTable = Nothing
          , constraintKeyForeignColumns = Nothing
          }
   in ConstraintDefinition
        { _constraintSqlExpr = expr
        , _constraintMigrationKey = migrationKey
        }

{- |
  A 'ForeignReference' represents one part of a foreign key. The entire foreign
  key may comprise multiple columns. The 'ForeignReference' defines a single
  column in the key and which column it references in the foreign table.
-}
data ForeignReference = ForeignReference
  { localFieldName :: FieldDefinition.FieldName
  , foreignFieldName :: FieldDefinition.FieldName
  }

{- |
  Constructs a 'ForeignReference'
-}
foreignReference ::
  -- | The name of the field in the table with the constraint
  FieldDefinition.FieldName ->
  -- | The name of the field in the foreign table that the local field references
  FieldDefinition.FieldName ->
  ForeignReference
foreignReference localName foreignName =
  ForeignReference
    { localFieldName = localName
    , foreignFieldName = foreignName
    }

{- |
  Builds a 'ConstraintDefinition' for a @FOREIGN KEY@ constraint.
-}
foreignKeyConstraint ::
  -- | Identifier of the table referenced by the foreign key
  TableIdentifier.TableIdentifier ->
  -- | The columns constrained by the foreign key and those that they reference in the foreign table
  NonEmpty ForeignReference ->
  ConstraintDefinition
foreignKeyConstraint foreignTableId foreignReferences =
  let localFieldNames =
        localFieldName <$> foreignReferences

      foreignFieldNames =
        foreignFieldName <$> foreignReferences

      expr =
        Expr.foreignKeyConstraint
          (fmap FieldDefinition.fieldNameToColumnName localFieldNames)
          (TableIdentifier.tableIdQualifiedName foreignTableId)
          (fmap FieldDefinition.fieldNameToColumnName foreignFieldNames)

      migrationKey =
        ConstraintMigrationKey
          { constraintKeyType = ForeignKeyConstraint
          , constraintKeyColumns = Just (NEL.toList localFieldNames)
          , constraintKeyForeignTable = Just foreignTableId
          , constraintKeyForeignColumns = Just (NEL.toList foreignFieldNames)
          }
   in ConstraintDefinition
        { _constraintSqlExpr = expr
        , _constraintMigrationKey = migrationKey
        }