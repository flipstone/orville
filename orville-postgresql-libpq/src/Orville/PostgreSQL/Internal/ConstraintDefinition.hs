module Orville.PostgreSQL.Internal.ConstraintDefinition
  ( ConstraintDefinition,
    uniqueConstraint,
    ConstraintMigrationKey (ConstraintMigrationKey, constrainedColumns),
    constraintMigrationKey,
    constraintSqlExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDef

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
  { constrainedColumns :: [FieldDef.FieldName]
  }
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
uniqueConstraint :: NonEmpty FieldDef.FieldName -> ConstraintDefinition
uniqueConstraint columnNames =
  let expr =
        Expr.uniqueConstraint . fmap FieldDef.fieldNameToColumnName $ columnNames

      metadata =
        ConstraintMigrationKey
          { constrainedColumns = NEL.toList columnNames
          }
   in ConstraintDefinition
        { _constraintSqlExpr = expr
        , _constraintMigrationKey = metadata
        }
