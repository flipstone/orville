{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Schema.ConstraintDefinition
  ( ConstraintDefinition
  , uniqueConstraint
  , foreignKeyConstraint
  , foreignKeyConstraintWithOptions
  , ForeignReference (ForeignReference, localFieldName, foreignFieldName)
  , foreignReference
  , ConstraintMigrationKey (ConstraintMigrationKey, constraintKeyType, constraintKeyColumns, constraintKeyForeignTable, constraintKeyForeignColumns, constraintKeyForeignKeyOnUpdateAction, constraintKeyForeignKeyOnDeleteAction)
  , ConstraintKeyType (UniqueConstraint, ForeignKeyConstraint)
  , constraintMigrationKey
  , constraintSqlExpr
  , ForeignKeyAction (..)
  , ForeignKeyOptions (foreignKeyOptionsOnDelete, foreignKeyOptionsOnUpdate)
  , defaultForeignKeyOptions
  , TableConstraints
  , emptyTableConstraints
  , addConstraint
  , tableConstraintDefinitions
  , tableConstraintKeys
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldName as FieldName
import qualified Orville.PostgreSQL.Schema.TableIdentifier as TableIdentifier

{- |
  A collection of constraints to be added to a table. This collection is
  indexed by 'ConstraintMigrationKey'. If multiple constraints with the same
  'ConstraintMigrationKey' are added, the most recently-added one will be kept
  and the previous one dropped.

@since 1.0.0.0
-}
newtype TableConstraints
  = TableConstraints (Map.Map ConstraintMigrationKey ConstraintDefinition)
  deriving (Semigroup, Monoid)

{- |
  Constructs an empty 'TableConstraints'.

@since 1.0.0.0
-}
emptyTableConstraints :: TableConstraints
emptyTableConstraints = TableConstraints Map.empty

{- |
  Adds a 'ConstraintDefinition' to an existing 'TableConstraints'. If a
  constraint already exists with the same 'ConstraintMigrationKey', it is
  replaced with the new constraint.

@since 1.0.0.0
-}
addConstraint :: ConstraintDefinition -> TableConstraints -> TableConstraints
addConstraint constraint (TableConstraints constraintMap) =
  TableConstraints $
    Map.insert
      (constraintMigrationKey constraint)
      constraint
      constraintMap

{- |
  Gets the list of 'ConstraintDefinition's that have been added to the
  'TableConstraints'.

@since 1.0.0.0
-}
tableConstraintKeys :: TableConstraints -> Set.Set ConstraintMigrationKey
tableConstraintKeys (TableConstraints constraints) =
  Map.keysSet constraints

{- |
  Gets the list of 'ConstraintDefinition's that have been added to the
  'TableConstraints'.

@since 1.0.0.0
-}
tableConstraintDefinitions :: TableConstraints -> [ConstraintDefinition]
tableConstraintDefinitions (TableConstraints constraints) =
  Map.elems constraints

{- |
  Defines a constraint that can be added to a
  'Orville.PostgreSQL.TableDefinition'. Use one of the constructor functions
  below (such as 'uniqueConstraint') to construct the constraint definition you
  wish to have and then use 'Orville.PostgreSQL.addTableConstraints' to add
  them to your table definition. Orville will then add the constraint next time
  you run auto-migrations.

@since 1.0.0.0
-}
data ConstraintDefinition = ConstraintDefinition
  { _constraintSqlExpr :: Expr.TableConstraint
  , _constraintMigrationKey :: ConstraintMigrationKey
  }

{- |
  The key used by Orville to determine whether a constraint should be added to
  a table when performing auto-migrations. For most use cases, the constructor
  functions that build a 'ConstraintDefinition' will create this automatically
  for you.

@since 1.0.0.0
-}
data ConstraintMigrationKey = ConstraintMigrationKey
  { constraintKeyType :: ConstraintKeyType
  , constraintKeyColumns :: Maybe [FieldName.FieldName]
  , constraintKeyForeignTable :: Maybe TableIdentifier.TableIdentifier
  , constraintKeyForeignColumns :: Maybe [FieldName.FieldName]
  , constraintKeyForeignKeyOnUpdateAction :: Maybe ForeignKeyAction
  , constraintKeyForeignKeyOnDeleteAction :: Maybe ForeignKeyAction
  }
  deriving (Eq, Ord, Show)

{- |
  The kind of constraint that is described by a 'ConstraintMigrationKey' (e.g.
  unique, foreign key).

@since 1.0.0.0
-}
data ConstraintKeyType
  = UniqueConstraint
  | ForeignKeyConstraint
  deriving (Eq, Ord, Show)

{- |
  Gets the 'ConstraintMigrationKey' for the 'ConstraintDefinition'.

@since 1.0.0.0
-}
constraintMigrationKey :: ConstraintDefinition -> ConstraintMigrationKey
constraintMigrationKey = _constraintMigrationKey

{- |
  Gets the SQL expression that will be used to add the constraint to the table.

@since 1.0.0.0
-}
constraintSqlExpr :: ConstraintDefinition -> Expr.TableConstraint
constraintSqlExpr = _constraintSqlExpr

{- |
  Constructs a 'ConstraintDefinition' for a @UNIQUE@ constraint on the given
  columns.

@since 1.0.0.0
-}
uniqueConstraint :: NonEmpty FieldName.FieldName -> ConstraintDefinition
uniqueConstraint fieldNames =
  let
    expr =
      Expr.uniqueConstraint . fmap FieldName.fieldNameToColumnName $ fieldNames

    migrationKey =
      ConstraintMigrationKey
        { constraintKeyType = UniqueConstraint
        , constraintKeyColumns = Just (NEL.toList fieldNames)
        , constraintKeyForeignTable = Nothing
        , constraintKeyForeignColumns = Nothing
        , constraintKeyForeignKeyOnUpdateAction = Nothing
        , constraintKeyForeignKeyOnDeleteAction = Nothing
        }
  in
    ConstraintDefinition
      { _constraintSqlExpr = expr
      , _constraintMigrationKey = migrationKey
      }

{- |
  A 'ForeignReference' represents one part of a foreign key. The entire foreign
  key may comprise multiple columns. The 'ForeignReference' defines a single
  column in the key and which column it references in the foreign table.

@since 1.0.0.0
-}
data ForeignReference = ForeignReference
  { localFieldName :: FieldName.FieldName
  , foreignFieldName :: FieldName.FieldName
  }

{- |
  Constructs a 'ForeignReference'.

@since 1.0.0.0
-}
foreignReference ::
  -- | The name of the field in the table with the constraint.
  FieldName.FieldName ->
  -- | The name of the field in the foreign table that the local field references.
  FieldName.FieldName ->
  ForeignReference
foreignReference localName foreignName =
  ForeignReference
    { localFieldName = localName
    , foreignFieldName = foreignName
    }

{- |
  Defines the options for a foreign key constraint. To construct
  'ForeignKeyOptions', perform a record update on 'defaultForeignKeyOptions'.

@since 1.0.0.0
-}
data ForeignKeyOptions = ForeignKeyOptions
  { foreignKeyOptionsOnUpdate :: ForeignKeyAction
  -- ^ The @ON UPDATE@ action for the foreign key.
  , foreignKeyOptionsOnDelete :: ForeignKeyAction
  -- ^ The @ON DELETE@ action for the foreign key.
  }

{- |
  The default 'ForeignKeyOptions', containing 'NoAction' for both
  'foreignKeyOptionsOnUpdate' and 'foreignKeyOptionsOnDelete'.

@since 1.0.0.0
-}
defaultForeignKeyOptions :: ForeignKeyOptions
defaultForeignKeyOptions =
  ForeignKeyOptions
    { foreignKeyOptionsOnUpdate = NoAction
    , foreignKeyOptionsOnDelete = NoAction
    }

{- |
  The actions that can be set on 'ForeignKeyOptions'.

@since 1.0.0.0
-}
data ForeignKeyAction
  = NoAction
  | Restrict
  | Cascade
  | SetNull
  | SetDefault
  deriving (Show, Eq, Ord)

foreignKeyActionToExpr :: ForeignKeyAction -> Maybe Expr.ForeignKeyActionExpr
foreignKeyActionToExpr action = case action of
  NoAction -> Nothing
  Restrict -> Just Expr.restrictExpr
  Cascade -> Just Expr.cascadeExpr
  SetNull -> Just Expr.setNullExpr
  SetDefault -> Just Expr.setDefaultExpr

{- |
  Builds a 'ConstraintDefinition' for a @FOREIGN KEY@ constraint.

@since 1.0.0.0
-}
foreignKeyConstraint ::
  -- | Identifier of the table referenced by the foreign key.
  TableIdentifier.TableIdentifier ->
  -- | The columns constrained by the foreign key and those that they reference in the foreign table.
  NonEmpty ForeignReference ->
  ConstraintDefinition
foreignKeyConstraint foreignTableId foreignReferences =
  foreignKeyConstraintWithOptions foreignTableId foreignReferences defaultForeignKeyOptions

{- |
  Builds a 'ConstraintDefinition' for a @FOREIGN KEY@ constraint, with
  ON UPDATE and ON DELETE actions.

@since 1.0.0.0
-}
foreignKeyConstraintWithOptions ::
  -- | Identifier of the table referenced by the foreign key.
  TableIdentifier.TableIdentifier ->
  -- | The columns constrained by the foreign key and those that they reference in the foreign table.
  NonEmpty ForeignReference ->
  ForeignKeyOptions ->
  ConstraintDefinition
foreignKeyConstraintWithOptions foreignTableId foreignReferences options =
  let
    localFieldNames =
      localFieldName <$> foreignReferences

    foreignFieldNames =
      foreignFieldName <$> foreignReferences

    updateAction = foreignKeyOptionsOnUpdate options

    deleteAction = foreignKeyOptionsOnDelete options

    onUpdateExpr = fmap Expr.foreignKeyUpdateActionExpr $ foreignKeyActionToExpr updateAction

    onDeleteExpr = fmap Expr.foreignKeyDeleteActionExpr $ foreignKeyActionToExpr deleteAction

    expr =
      Expr.foreignKeyConstraint
        (fmap FieldName.fieldNameToColumnName localFieldNames)
        (TableIdentifier.tableIdQualifiedName foreignTableId)
        (fmap FieldName.fieldNameToColumnName foreignFieldNames)
        onUpdateExpr
        onDeleteExpr

    migrationKey =
      ConstraintMigrationKey
        { constraintKeyType = ForeignKeyConstraint
        , constraintKeyColumns = Just (NEL.toList localFieldNames)
        , constraintKeyForeignTable = Just foreignTableId
        , constraintKeyForeignColumns = Just (NEL.toList foreignFieldNames)
        , constraintKeyForeignKeyOnUpdateAction = Just updateAction
        , constraintKeyForeignKeyOnDeleteAction = Just deleteAction
        }
  in
    ConstraintDefinition
      { _constraintSqlExpr = expr
      , _constraintMigrationKey = migrationKey
      }
