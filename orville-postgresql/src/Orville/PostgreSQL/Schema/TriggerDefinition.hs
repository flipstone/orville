{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Schema.TriggerDefinition
  ( TriggerDefinition
  , beforeInsert
  , afterInsert
  , beforeUpdate
  , afterUpdate
  , beforeDelete
  , afterDelete
  , mkTriggerDefinition
  , TriggerMigrationKey (NamedTriggerKey)
  , triggerMigrationKey
  , mkCreateTriggerExpr
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Orville.PostgreSQL.Expr as Expr

{- |
  Defines a trigger that can be added to a
  'Orville.PostgreSQL.TableDefinition'. Use one of the constructor functions
  below (such as 'mkNamedTriggerDefinition') to construct the constraint
  definition you wish to have and then use 'Orville.PostgreSQL.addTableTriggers'
  to add them to your table definition. Orville will then add the trigger next
  time you run auto-migrations.

  Note that currently Orville will only add triggers when it sees that no trigger
  with the same name exists. If you want to change the definition of the trigger
  you should also change the name that you specify when making the 'TriggerDefinition'
  so that Orville will drop the old trigger and create a new trigger with the new
  name.

@since 1.1.0.0
-}
data TriggerDefinition = TriggerDefinition
  { i_triggerMigrationKey :: TriggerMigrationKey
  , i_triggerCreateExpr ::
      Maybe Expr.OrReplace ->
      Expr.QualifiedOrUnqualified Expr.TableName ->
      Expr.CreateTriggerExpr
  }

{- |
  Orville uses 'TriggerMigration' values while performing auto migrations to
  determine whether an trigger needs to be added or dropped. For most use cases
  the constructor functions that build an 'TriggerDefinition' will create this
  automatically for you.

@since 1.1.0.0
-}
newtype TriggerMigrationKey
  = NamedTriggerKey String
  deriving
    ( -- | @since 1.1.0.0
      Eq
    , -- | @since 1.1.0.0
      Ord
    )

{- |
  Gets the 'TriggerMigrationKey' for the 'TriggerDefinition'

@since 1.1.0.0
-}
triggerMigrationKey :: TriggerDefinition -> TriggerMigrationKey
triggerMigrationKey =
  i_triggerMigrationKey

{- |
  Builds a 'TriggerDefinition' that will execute the named function for each row
  being inserted before it is inserted.

@since 1.1.0.0
-}
beforeInsert :: String -> Expr.QualifiedOrUnqualified Expr.FunctionName -> TriggerDefinition
beforeInsert name functionName =
  mkTriggerDefinition
    name
    Expr.triggerBefore
    (Expr.triggerOnInsert :| [])
    Expr.triggerForEachRow
    functionName

{- |
  Builds a 'TriggerDefinition' that will execute the named function for each row
  being inserted after it is inserted.

@since 1.1.0.0
-}
afterInsert :: String -> Expr.QualifiedOrUnqualified Expr.FunctionName -> TriggerDefinition
afterInsert name functionName =
  mkTriggerDefinition
    name
    Expr.triggerAfter
    (Expr.triggerOnInsert :| [])
    Expr.triggerForEachRow
    functionName

{- |
  Builds a 'TriggerDefinition' that will execute the named function for each row
  being updated before it is updated.

@since 1.1.0.0
-}
beforeUpdate :: String -> Expr.QualifiedOrUnqualified Expr.FunctionName -> TriggerDefinition
beforeUpdate name functionName =
  mkTriggerDefinition
    name
    Expr.triggerBefore
    (Expr.triggerOnUpdate :| [])
    Expr.triggerForEachRow
    functionName

{- |
  Builds a 'TriggerDefinition' that will execute the named function for each row
  being updated after it is updated.

@since 1.1.0.0
-}
afterUpdate :: String -> Expr.QualifiedOrUnqualified Expr.FunctionName -> TriggerDefinition
afterUpdate name functionName =
  mkTriggerDefinition
    name
    Expr.triggerAfter
    (Expr.triggerOnUpdate :| [])
    Expr.triggerForEachRow
    functionName

{- |
  Builds a 'TriggerDefinition' that will execute the named function for each row
  being deleted before it is deleted.

@since 1.1.0.0
-}
beforeDelete :: String -> Expr.QualifiedOrUnqualified Expr.FunctionName -> TriggerDefinition
beforeDelete name functionName =
  mkTriggerDefinition
    name
    Expr.triggerBefore
    (Expr.triggerOnDelete :| [])
    Expr.triggerForEachRow
    functionName

{- |
  Builds a 'TriggerDefinition' that will execute the named function for each row
  being deleted after it is deleted.

@since 1.1.0.0
-}
afterDelete :: String -> Expr.QualifiedOrUnqualified Expr.FunctionName -> TriggerDefinition
afterDelete name functionName =
  mkTriggerDefinition
    name
    Expr.triggerAfter
    (Expr.triggerOnDelete :| [])
    Expr.triggerForEachRow
    functionName

{- |
  Constructs a 'TriggerDefinition' definining a trigger with the specified name.
  Note that orville is currently not capable of migrating triggers based on their
  structure. The trigger will be created if no trigger exists on the table with
  the specificed name. If a trigger already exists with the same name on the table
  then nothing will be done.

@since 1.1.0.0
-}
mkTriggerDefinition ::
  String ->
  Expr.TriggerTiming ->
  NonEmpty Expr.TriggerEvent ->
  Expr.TriggerFireScope ->
  Expr.QualifiedOrUnqualified Expr.FunctionName ->
  TriggerDefinition
mkTriggerDefinition name timing events fireScope functionName =
  TriggerDefinition
    { i_triggerMigrationKey = NamedTriggerKey name
    , i_triggerCreateExpr =
        \orReplace tableName ->
          Expr.createTrigger
            orReplace
            (Expr.triggerName name)
            timing
            events
            tableName
            fireScope
            functionName
    }

{- |
  Builds a 'Expr.CreateTriggerExpr' that will create a SQL trigger matching
  the given 'TriggerDefinition' when it is executed.

@since 1.1.0.0
-}
mkCreateTriggerExpr ::
  TriggerDefinition ->
  Maybe Expr.OrReplace ->
  Expr.QualifiedOrUnqualified Expr.TableName ->
  Expr.CreateTriggerExpr
mkCreateTriggerExpr =
  i_triggerCreateExpr
