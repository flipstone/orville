{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Vacuum
  ( VacuumExpr
  , vacuumExpr
  , VacuumOption
  , vacuumFull
  , vacuumFreeze
  , vacuumVerbose
  , vacuumAnalyze
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Expr.Name (Qualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a @VACUUM@ statement. E.G.

> VACUUM foo

'VacuumExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype VacuumExpr
  = VacuumExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
  Constructs a 'VacuumExpr' with the given vacuum options on the given tables.

  @since 1.1.0.0
-}
vacuumExpr :: [VacuumOption] -> NonEmpty (Qualified TableName) -> VacuumExpr
vacuumExpr vacuumOptions tables =
  let
    optionsWithSpaceRawSql =
      case vacuumOptions of
        [] ->
          RawSql.space
        opts ->
          RawSql.space
            <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace opts)
            <> RawSql.space
  in
    VacuumExpr $
      RawSql.fromString "VACUUM"
        <> optionsWithSpaceRawSql
        <> RawSql.intercalate RawSql.commaSpace tables

newtype VacuumOption
  = VacuumOption RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
  Constructs a 'VaccumOption' that will instruct if the vacuum should be "full".

  @since 1.1.0.0
-}
vacuumFull :: Bool -> VacuumOption
vacuumFull bool =
  VacuumOption $ RawSql.fromString "FULL " <> boolRawSql bool

{- | Constructs a 'VaccumOption' that will instruct if the vacuum should be agressive in "freezing" of
  tuples.

  @since 1.1.0.0
-}
vacuumFreeze :: Bool -> VacuumOption
vacuumFreeze bool =
  VacuumOption $ RawSql.fromString "FREEZE " <> boolRawSql bool

{- |
  Constructs a 'VaccumOption' that will instruct if the vacuum should be verbose.

  @since 1.1.0.0
-}
vacuumVerbose :: Bool -> VacuumOption
vacuumVerbose bool =
  VacuumOption $ RawSql.fromString "VERBOSE " <> boolRawSql bool

{- | Constructs a 'VaccumOption' that will instruct if the vacuum should "analyze" to update statitics
  used by the PostgreSQL query planner.

  @since 1.1.0.0
-}
vacuumAnalyze :: Bool -> VacuumOption
vacuumAnalyze bool =
  VacuumOption $ RawSql.fromString "ANALYZE " <> boolRawSql bool

boolRawSql :: Bool -> RawSql.RawSql
boolRawSql bool =
  if bool then RawSql.fromString "TRUE" else RawSql.fromString "FALSE"
