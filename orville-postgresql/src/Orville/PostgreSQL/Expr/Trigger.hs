{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Trigger
  ( DropTriggerExpr
  , dropTrigger
  , CreateTriggerExpr
  , createTrigger
  , TriggerTiming
  , triggerBefore
  , triggerAfter
  , triggerInsteadOf
  , TriggerEvent
  , triggerOnInsert
  , triggerOnUpdate
  , triggerOnUpdateOf
  , triggerOnDelete
  , triggerOnTruncate
  , TriggerFireScope
  , triggerForEachRow
  , triggerForEachStatement
  ) where

import qualified Data.List.NonEmpty as NEL
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.Name (ColumnName, FunctionName, QualifiedOrUnqualified, TableName, TriggerName)
import Orville.PostgreSQL.Expr.OrReplace (OrReplace)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL "DROP TRIGGER" statement. E.G.

> DROP TRIGGER my_trigger ON my_table

'DropTriggerExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype DropTriggerExpr
  = DropTriggerExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Constructs a SQL @DROP TRIGGER@ statement from the trigger name and table name

@since 1.1.0.0
-}
dropTrigger ::
  Maybe IfExists ->
  TriggerName ->
  QualifiedOrUnqualified TableName ->
  DropTriggerExpr
dropTrigger maybeIfExists name tableName =
  DropTriggerExpr $
    RawSql.intercalate
      RawSql.space
      ( catMaybes
          [ Just (RawSql.fromString "DROP TRIGGER")
          , fmap RawSql.toRawSql maybeIfExists
          , Just (RawSql.toRawSql name)
          , Just (RawSql.fromString "ON")
          , Just (RawSql.toRawSql tableName)
          ]
      )

{- | Type to represent a SQL "CREATE TRIGGER" statement. E.G.

> CREATE TRIGGER my_trigger BEFORE UPDATE ON my_table EXECUTE PROCEDURE my_trigger_function

'CreateTriggerExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype CreateTriggerExpr
  = CreateTriggerExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Constructs a SQL @CREATE TRIGGER@ statement from the trigger name and its defining
attributes.

Note: Orville does not currently support creating triggers with arguments.

Note: Before PostgreSQL 14, there is no @CREATE OR REPLACE TRIGGER@ syntax in
PsotgreSQL. If you're using a version of PostgreSQL prior to 14 then you must
specify 'Nothing' for the 'OrReplace' property.

@since 1.1.0.0
-}
createTrigger ::
  Maybe OrReplace ->
  TriggerName ->
  TriggerTiming ->
  NEL.NonEmpty TriggerEvent ->
  QualifiedOrUnqualified TableName ->
  TriggerFireScope ->
  QualifiedOrUnqualified FunctionName ->
  CreateTriggerExpr
createTrigger maybeOrReplace name timing events tableName fireScope functionName =
  CreateTriggerExpr $
    RawSql.intercalate
      RawSql.space
      ( catMaybes
          [ Just $ RawSql.fromString "CREATE"
          , RawSql.toRawSql <$> maybeOrReplace
          , Just $ RawSql.fromString "TRIGGER"
          , Just $ RawSql.toRawSql name
          , Just $ RawSql.toRawSql timing
          , Just $ RawSql.intercalate (RawSql.fromString " OR ") events
          , Just $ RawSql.fromString "ON"
          , Just $ RawSql.toRawSql tableName
          , Just $ RawSql.toRawSql fireScope
          , Just $ RawSql.fromString "EXECUTE FUNCTION"
          , Just $ RawSql.toRawSql functionName
          , Just $ RawSql.fromString "()" -- we don't currently support arguments
          ]
      )

{- | Type to represent the time at which a trigger will fire in releation to the
event that caused it. E.G. the @BEFORE@ in

> CREATE TRIGGER my_trigger BEFORE UPDATE ON my_table FOR EACH ROW EXECUTE PROCEDURE my_trigger_function

'TriggerTiming' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype TriggerTiming
  = TriggerTiming RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The @BEFORE@ 'TriggerTiming'.

@since 1.1.0.0
-}
triggerBefore :: TriggerTiming
triggerBefore =
  TriggerTiming (RawSql.fromString "BEFORE")

{- | The @AFTER@ 'TriggerTiming'.

@since 1.1.0.0
-}
triggerAfter :: TriggerTiming
triggerAfter =
  TriggerTiming (RawSql.fromString "AFTER")

{- | The @INSTEAD OF@ 'TriggerTiming'.

@since 1.1.0.0
-}
triggerInsteadOf :: TriggerTiming
triggerInsteadOf =
  TriggerTiming (RawSql.fromString "INSTEAD OF")

{- | Type to represent the an event that will cause a trigger to fire in a SQL "CREATE
TRIGGER" statement. E.G. the @UPDATE@ in

> CREATE TRIGGER my_trigger BEFORE UPDATE ON my_table FOR EACH ROW EXECUTE PROCEDURE my_trigger_function

'TriggerEvent' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype TriggerEvent
  = TriggerEvent RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The @INSERT@ 'TriggerEvent'

@since 1.1.0.0
-}
triggerOnInsert :: TriggerEvent
triggerOnInsert =
  TriggerEvent (RawSql.fromString "INSERT")

{- | The @UPDATE@ 'TriggerEvent'

@since 1.1.0.0
-}
triggerOnUpdate :: TriggerEvent
triggerOnUpdate =
  TriggerEvent (RawSql.fromString "UPDATE")

{- | The @UPDATE OF@ 'TriggerEvent'. This causes the trigger only to fire when on of
the specified columns is mentioned of affected by an update.

@since 1.1.0.0
-}
triggerOnUpdateOf :: NEL.NonEmpty ColumnName -> TriggerEvent
triggerOnUpdateOf columnNames =
  TriggerEvent $
    RawSql.fromString "UPDATE OF"
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql columnNames)

{- | The @DELETE@ 'TriggerEvent'

@since 1.1.0.0
-}
triggerOnDelete :: TriggerEvent
triggerOnDelete =
  TriggerEvent (RawSql.fromString "DELETE")

{- | The @TRUNCATE@ 'TriggerEvent'

@since 1.1.0.0
-}
triggerOnTruncate :: TriggerEvent
triggerOnTruncate =
  TriggerEvent (RawSql.fromString "TRUNCATE")

{- | Type to represent the part of a SQL "CREATE TRIGGER" statement that indicates whether
the trigger will be fired once for each row or once for each statement. E.G. the
@FOR EACH ROW@ in

> CREATE TRIGGER my_trigger BEFORE UPDATE ON my_table FOR EACH ROW EXECUTE PROCEDURE my_trigger_function

'TriggerFireScope' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype TriggerFireScope
  = TriggerFireScope RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The @FOR EACH ROW@ 'TriggerFireScope'

@since 1.1.0.0
-}
triggerForEachRow :: TriggerFireScope
triggerForEachRow =
  TriggerFireScope (RawSql.fromString "FOR EACH ROW")

{- | The @FOR EACH STATEMENT@ 'TriggerFireScope'

@since 1.1.0.0
-}
triggerForEachStatement :: TriggerFireScope
triggerForEachStatement =
  TriggerFireScope (RawSql.fromString "FOR EACH STATEMENT")
