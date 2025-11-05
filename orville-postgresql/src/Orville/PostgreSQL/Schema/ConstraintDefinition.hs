{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Schema.ConstraintDefinition
  ( ConstraintDefinition
  , checkConstraint
  , uniqueConstraint
  , namedConstraint
  , foreignKeyConstraint
  , foreignKeyConstraintWithOptions
  , ForeignReference (ForeignReference, localFieldName, foreignFieldName)
  , foreignReference
  , ConstraintMigrationKey (NamedBasedConstraint, AttributeBasedConstraint)
  , AttributeBasedConstraintKey (UniqueConstraint, ForeignKeyConstraint)
  , UniqueConstraintMigrationData
  , ForeignKeyConstraintMigrationData (..)
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
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Schema.ConstraintIdentifier as ConstraintIdentifier
import qualified Orville.PostgreSQL.Schema.TableIdentifier as TableIdentifier

{- | A collection of constraints to be added to a table. This collection is
  indexed by 'ConstraintMigrationKey'. If multiple constraints with the same
  'ConstraintMigrationKey' are added, the most recently-added one will be kept
  and the previous one dropped.

@since 1.0.0.0
-}
newtype TableConstraints
  = TableConstraints (Map.Map ConstraintMigrationKey ConstraintDefinition)
  deriving
    ( -- | @since 1.0.0.0
      Semigroup
    , -- | @since 1.0.0.0
      Monoid
    )

{- | Constructs an empty 'TableConstraints'.

@since 1.0.0.0
-}
emptyTableConstraints :: TableConstraints
emptyTableConstraints = TableConstraints Map.empty

{- | Adds a 'ConstraintDefinition' to an existing 'TableConstraints'. If a
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

{- | Gets the list of 'ConstraintDefinition's that have been added to the
  'TableConstraints'.

@since 1.0.0.0
-}
tableConstraintKeys :: TableConstraints -> Set.Set ConstraintMigrationKey
tableConstraintKeys (TableConstraints constraints) =
  Map.keysSet constraints

{- | Gets the list of 'ConstraintDefinition's that have been added to the
  'TableConstraints'.

@since 1.0.0.0
-}
tableConstraintDefinitions :: TableConstraints -> [ConstraintDefinition]
tableConstraintDefinitions (TableConstraints constraints) =
  Map.elems constraints

{- | Defines a constraint that can be added to a
  'Orville.PostgreSQL.TableDefinition'. Use one of the constructor functions
  below (such as 'uniqueConstraint') to construct the constraint definition you
  wish to have and then use 'Orville.PostgreSQL.addTableConstraints' to add
  them to your table definition. Orville will then add the constraint next time
  you run auto-migrations.

@since 1.0.0.0
-}
data ConstraintDefinition = ConstraintDefinition
  { i_constraintSqlExpr :: Expr.TableConstraint
  , i_constraintMigrationKey :: ConstraintMigrationKey
  }

{- | The key used by Orville to determine whether a constraint should be added to
  a table when performing auto-migrations. For most use cases, the constructor
  functions that build a 'ConstraintDefinition' will create this automatically
  for you. A named based constraint migration key will only migrate the constraint if the
  constraint name changes.

@since 1.0.0.0
-}
data ConstraintMigrationKey
  = NamedBasedConstraint ConstraintIdentifier.ConstraintIdentifier
  | AttributeBasedConstraint AttributeBasedConstraintKey
  deriving
    ( -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Ord
    , -- | @since 1.0.0.0
      Show
    )

{- | The data used by Orville to determine whether a constraint should be added to
  a table when performing auto-migrations. For most use cases, the constructor
  functions that build a 'ConstraintDefinition' will create this automatically
  for you.

@since 1.1.0.0.3
-}
data ForeignKeyConstraintMigrationData = ForeignKeyConstraintMigrationData
  { foreignKeyConstraintMigrationDataColumns :: [FieldName.FieldName]
  , foreignKeyConstraintMigrationDataForeignTable :: Maybe TableIdentifier.TableIdentifier
  , foreignKeyConstraintMigrationDataForeignColumns :: [FieldName.FieldName]
  , foreignKeyConstraintMigrationDataForeignKeyOnUpdateAction :: Maybe ForeignKeyAction
  , foreignKeyConstraintMigrationDataForeignKeyOnDeleteAction :: Maybe ForeignKeyAction
  }
  deriving
    ( -- | @since 1.1.0.0.3
      Eq
    , -- | @since 1.1.0.0.3
      Ord
    , -- | @since 1.1.0.0.3
      Show
    )

{- | The data used by Orville to determine whether a constraint should be added to
  a table when performing auto-migrations. For most use cases, the constructor
  functions that build a 'ConstraintDefinition' will create this automatically
  for you.

@since 1.1.0.0.3
-}
type UniqueConstraintMigrationData = [FieldName.FieldName]

{- | The data used by Orville to determine whether a constraint should be added to
  a table when performing auto-migrations. For most use cases, the constructor
  functions that build a 'ConstraintDefinition' will create this automatically
  for you.

@since 1.1.0.0.3
-}
data AttributeBasedConstraintKey
  = UniqueConstraint UniqueConstraintMigrationData
  | ForeignKeyConstraint ForeignKeyConstraintMigrationData
  deriving
    ( -- | @since 1.1.0.0.3
      Eq
    , -- | @since 1.1.0.0.3
      Ord
    , -- | @since 1.1.0.0.3
      Show
    )

{- | Gets the 'ConstraintMigrationKey' for the 'ConstraintDefinition'.

@since 1.0.0.0
-}
constraintMigrationKey :: ConstraintDefinition -> ConstraintMigrationKey
constraintMigrationKey = i_constraintMigrationKey

{- | Gets the SQL expression that will be used to add the constraint to the table.

@since 1.0.0.0
-}
constraintSqlExpr :: ConstraintDefinition -> Expr.TableConstraint
constraintSqlExpr = i_constraintSqlExpr

{- | Constructs a 'ConstraintDefinition' for a @CHECK@ constraint with the given name and boolean expression.

@since 1.2.0.0
-}
checkConstraint :: ConstraintIdentifier.ConstraintIdentifier -> RawSql.RawSql -> ConstraintDefinition
checkConstraint constraintIdentifier constraintConditionExpr =
  let
    expr = Expr.checkConstraint (ConstraintIdentifier.constraintIdUnqualifiedName constraintIdentifier) constraintConditionExpr
    migrationKey = NamedBasedConstraint constraintIdentifier
  in
    ConstraintDefinition
      { i_constraintSqlExpr = expr
      , i_constraintMigrationKey = migrationKey
      }

{- | Constructs a 'ConstraintDefinition' for a @UNIQUE@ constraint on the given
  columns.

@since 1.0.0.0
-}
uniqueConstraint :: NonEmpty FieldName.FieldName -> ConstraintDefinition
uniqueConstraint fieldNames =
  let
    expr = Expr.uniqueConstraint . fmap FieldName.fieldNameToColumnName $ fieldNames
    migrationKey = AttributeBasedConstraint . UniqueConstraint $ NEL.toList fieldNames
  in
    ConstraintDefinition
      { i_constraintSqlExpr = expr
      , i_constraintMigrationKey = migrationKey
      }

{- | Allows very flexible support for constructing  a 'ConstraintDefinition' that will be automigrated
  based solely on its name.

  @since 1.1.0.0.3
-}
namedConstraint :: ConstraintIdentifier.ConstraintIdentifier -> RawSql.RawSql -> ConstraintDefinition
namedConstraint constraintIdentifier constraintExpr =
  let
    expr =
      Expr.namedConstraint
        (ConstraintIdentifier.constraintIdUnqualifiedName constraintIdentifier)
        constraintExpr
    migrationKey = NamedBasedConstraint constraintIdentifier
  in
    ConstraintDefinition
      { i_constraintSqlExpr = expr
      , i_constraintMigrationKey = migrationKey
      }

{- | A 'ForeignReference' represents one part of a foreign key. The entire foreign
  key may comprise multiple columns. The 'ForeignReference' defines a single
  column in the key and which column it references in the foreign table.

@since 1.0.0.0
-}
data ForeignReference = ForeignReference
  { localFieldName :: FieldName.FieldName
  , foreignFieldName :: FieldName.FieldName
  }

{- | Constructs a 'ForeignReference'.

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

{- | Defines the options for a foreign key constraint. To construct
  'ForeignKeyOptions', perform a record update on 'defaultForeignKeyOptions'.

@since 1.0.0.0
-}
data ForeignKeyOptions = ForeignKeyOptions
  { foreignKeyOptionsOnUpdate :: ForeignKeyAction
  -- ^ The @ON UPDATE@ action for the foreign key.
  , foreignKeyOptionsOnDelete :: ForeignKeyAction
  -- ^ The @ON DELETE@ action for the foreign key.
  }

{- | The default 'ForeignKeyOptions', containing 'NoAction' for both
  'foreignKeyOptionsOnUpdate' and 'foreignKeyOptionsOnDelete'.

@since 1.0.0.0
-}
defaultForeignKeyOptions :: ForeignKeyOptions
defaultForeignKeyOptions =
  ForeignKeyOptions
    { foreignKeyOptionsOnUpdate = NoAction
    , foreignKeyOptionsOnDelete = NoAction
    }

{- | The actions that can be set on 'ForeignKeyOptions'.

@since 1.0.0.0
-}
data ForeignKeyAction
  = NoAction
  | Restrict
  | Cascade
  | SetNull
  | SetDefault
  deriving
    ( -- | @since 1.0.0.0
      Show
    , -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Ord
    )

foreignKeyActionToExpr :: ForeignKeyAction -> Maybe Expr.ForeignKeyActionExpr
foreignKeyActionToExpr action = case action of
  NoAction -> Nothing
  Restrict -> Just Expr.restrictExpr
  Cascade -> Just Expr.cascadeExpr
  SetNull -> Just Expr.setNullExpr
  SetDefault -> Just Expr.setDefaultExpr

{- | Builds a 'ConstraintDefinition' for a @FOREIGN KEY@ constraint.

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

{- | Builds a 'ConstraintDefinition' for a @FOREIGN KEY@ constraint, with
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
      AttributeBasedConstraint $
        ForeignKeyConstraint
          ForeignKeyConstraintMigrationData
            { foreignKeyConstraintMigrationDataColumns = NEL.toList localFieldNames
            , foreignKeyConstraintMigrationDataForeignTable = Just foreignTableId
            , foreignKeyConstraintMigrationDataForeignColumns = NEL.toList foreignFieldNames
            , foreignKeyConstraintMigrationDataForeignKeyOnUpdateAction = Just updateAction
            , foreignKeyConstraintMigrationDataForeignKeyOnDeleteAction = Just deleteAction
            }
  in
    ConstraintDefinition
      { i_constraintSqlExpr = expr
      , i_constraintMigrationKey = migrationKey
      }
