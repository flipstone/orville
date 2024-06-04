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
  deriving (RawSql.SqlExpression)

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

newtype RowLockingStrengthExpr
  = RowLockingStrengthExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

updateStrength :: RowLockingStrengthExpr
updateStrength =
  RowLockingStrengthExpr $ RawSql.fromString "UPDATE"

noKeyUpdateStrength :: RowLockingStrengthExpr
noKeyUpdateStrength =
  RowLockingStrengthExpr $ RawSql.fromString "NO KEY UPDATE"

shareStrength :: RowLockingStrengthExpr
shareStrength =
  RowLockingStrengthExpr $ RawSql.fromString "SHARE"

keyShareStrength :: RowLockingStrengthExpr
keyShareStrength =
  RowLockingStrengthExpr $ RawSql.fromString "KEY SHARE"

newtype RowLockingOptionExpr
  = RowLockingOptionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

noWaitRowLockingOption :: RowLockingOptionExpr
noWaitRowLockingOption =
  RowLockingOptionExpr $ RawSql.fromString "NO WAIT"

skipLockedRowLockingOption :: RowLockingOptionExpr
skipLockedRowLockingOption =
  RowLockingOptionExpr $ RawSql.fromString "SKIP LOCKED"
