{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.RowLocking
  ( RowLockingClause
  , rowLockingClause
  , RowLockingStrengthExpr
  , updateStrength
  , noKeyUpdateStrength
  , shareStrength
  , keyShareStrength
  , RowLockingOptionExpr
  , noWaitRowLockingOption
  , skipLockedRowLockingOption
  )
where

import qualified Data.List.NonEmpty as NEL

import Orville.PostgreSQL.Expr.Internal.Name.TableName (TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent the row locking part of a SQL @SELECT@ query. E.G.

> FOR UPDATE

'RowLockingClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype RowLockingClause
  = RowLockingClause RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Builds a 'RowLockingClause' with the given strength. If provided the optional 'NEL.NonEmpty
   TableName' and 'RowLockingOptionExpr' will scope and set the lock option respectively.

@since 1.1.0.0
-}
rowLockingClause ::
  RowLockingStrengthExpr ->
  Maybe (NEL.NonEmpty TableName) ->
  Maybe RowLockingOptionExpr ->
  RowLockingClause
rowLockingClause strength mbTables mbOpt =
  RowLockingClause $
    RawSql.fromString "FOR "
      <> RawSql.toRawSql strength
      <> maybe
        mempty
        ( \tables ->
            RawSql.fromString " OF "
              <> RawSql.intercalate
                RawSql.commaSpace
                (fmap RawSql.toRawSql tables)
        )
        mbTables
      <> maybe mempty (\opt -> RawSql.space <> RawSql.toRawSql opt) mbOpt

{- | Type to represent the lock strength part of a row locking sub expression in a SQL @SELECT@
query. E.G.

> NO KEY UPDATE

'RowLockingStrengthExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype RowLockingStrengthExpr
  = RowLockingStrengthExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The @UPDATE@ lock strength

@since 1.1.0.0
-}
updateStrength :: RowLockingStrengthExpr
updateStrength =
  RowLockingStrengthExpr $ RawSql.fromString "UPDATE"

{- | The @NO KEY UPDATE@ lock strength

@since 1.1.0.0
-}
noKeyUpdateStrength :: RowLockingStrengthExpr
noKeyUpdateStrength =
  RowLockingStrengthExpr $ RawSql.fromString "NO KEY UPDATE"

{- | The @SHARE@ lock strength

@since 1.1.0.0
-}
shareStrength :: RowLockingStrengthExpr
shareStrength =
  RowLockingStrengthExpr $ RawSql.fromString "SHARE"

{- | The @KEY SHARE@ lock strength

@since 1.1.0.0
-}
keyShareStrength :: RowLockingStrengthExpr
keyShareStrength =
  RowLockingStrengthExpr $ RawSql.fromString "KEY SHARE"

{- | Type to represent the lock option part of a row locking sub expression in a SQL @SELECT@
query. E.G.

> SKIP LOCKED

'RowLockingOptionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype RowLockingOptionExpr
  = RowLockingOptionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The @NO WAIT@ lock option

@since 1.1.0.0
-}
noWaitRowLockingOption :: RowLockingOptionExpr
noWaitRowLockingOption =
  RowLockingOptionExpr $ RawSql.fromString "NO WAIT"

{- | The @SKIP LOCKED@ lock option

@since 1.1.0.0
-}
skipLockedRowLockingOption :: RowLockingOptionExpr
skipLockedRowLockingOption =
  RowLockingOptionExpr $ RawSql.fromString "SKIP LOCKED"
