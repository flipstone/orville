{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.SequenceDefinition
  ( CreateSequenceExpr,
    createSequenceExpr,
    AlterSequenceExpr,
    alterSequenceExpr,
    IncrementByExpr,
    incrementBy,
    MinValueExpr,
    minValue,
    noMinValue,
    MaxValueExpr,
    maxValue,
    noMaxValue,
    StartWithExpr,
    startWith,
    CacheExpr,
    cache,
    CycleExpr,
    cycle,
    noCycle,
    cycleIfTrue,
    DropSequenceExpr,
    dropSequenceExpr,
    nextVal,
    nextValFunction,
    currVal,
    currValFunction,
    setVal,
    setValFunction,
  )
where

-- to avoid conflict with cycle
import Prelude (Bool, Maybe (Just), fmap, ($), (.), (<>))

import Data.Int (Int64)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.Name (FunctionName, Qualified, SequenceName, functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, functionCall, valueExpression)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

{-
   From https://www.postgresql.org/docs/15/sql-createsequence.html

   @@
   CREATE [ { TEMPORARY | TEMP } | UNLOGGED ] SEQUENCE [ IF NOT EXISTS ] name
    [ AS data_type ]
    [ INCREMENT [ BY ] increment ]
    [ MINVALUE minvalue | NO MINVALUE ] [ MAXVALUE maxvalue | NO MAXVALUE ]
    [ START [ WITH ] start ] [ CACHE cache ] [ [ NO ] CYCLE ]
    [ OWNED BY { table_name.column_name | NONE } ]
   @@
-}

newtype CreateSequenceExpr
  = CreateSequenceExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

createSequenceExpr ::
  Qualified SequenceName ->
  Maybe IncrementByExpr ->
  Maybe MinValueExpr ->
  Maybe MaxValueExpr ->
  Maybe StartWithExpr ->
  Maybe CacheExpr ->
  Maybe CycleExpr ->
  CreateSequenceExpr
createSequenceExpr sequenceName mbIncrementBy mbMinValue mbMaxValue mbStartWith mbCache mbCycle =
  CreateSequenceExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "CREATE SEQUENCE")
      , Just (RawSql.toRawSql sequenceName)
      , fmap RawSql.toRawSql mbIncrementBy
      , fmap RawSql.toRawSql mbMinValue
      , fmap RawSql.toRawSql mbMaxValue
      , fmap RawSql.toRawSql mbStartWith
      , fmap RawSql.toRawSql mbCache
      , fmap RawSql.toRawSql mbCycle
      ]

{-
  From https://www.postgresql.org/docs/15/sql-altersequence.html

  @@
  ALTER SEQUENCE [ IF EXISTS ] name
    [ AS data_type ]
    [ INCREMENT [ BY ] increment ]
    [ MINVALUE minvalue | NO MINVALUE ] [ MAXVALUE maxvalue | NO MAXVALUE ]
    [ START [ WITH ] start ]
    [ RESTART [ [ WITH ] restart ] ]
    [ CACHE cache ] [ [ NO ] CYCLE ]
    [ OWNED BY { table_name.column_name | NONE } ]
  ALTER SEQUENCE [ IF EXISTS ] name SET { LOGGED | UNLOGGED }
  ALTER SEQUENCE [ IF EXISTS ] name OWNER TO { new_owner | CURRENT_ROLE | CURRENT_USER | SESSION_USER }
  ALTER SEQUENCE [ IF EXISTS ] name RENAME TO new_name
  ALTER SEQUENCE [ IF EXISTS ] name SET SCHEMA new_schema
  @@
-}

newtype AlterSequenceExpr
  = AlterSequenceExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

alterSequenceExpr ::
  Qualified SequenceName ->
  Maybe IncrementByExpr ->
  Maybe MinValueExpr ->
  Maybe MaxValueExpr ->
  Maybe StartWithExpr ->
  Maybe CacheExpr ->
  Maybe CycleExpr ->
  AlterSequenceExpr
alterSequenceExpr sequenceName mbIncrementBy mbMinValue mbMaxValue mbStartWith mbCache mbCycle =
  AlterSequenceExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "ALTER SEQUENCE")
      , Just (RawSql.toRawSql sequenceName)
      , fmap RawSql.toRawSql mbIncrementBy
      , fmap RawSql.toRawSql mbMinValue
      , fmap RawSql.toRawSql mbMaxValue
      , fmap RawSql.toRawSql mbStartWith
      , fmap RawSql.toRawSql mbCache
      , fmap RawSql.toRawSql mbCycle
      ]

newtype IncrementByExpr
  = IncrementByExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

incrementBy :: Int64 -> IncrementByExpr
incrementBy n =
  IncrementByExpr $
    RawSql.fromString "INCREMENT BY "
      <> RawSql.int64DecLiteral n

newtype MinValueExpr
  = MinValueExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

minValue :: Int64 -> MinValueExpr
minValue n =
  MinValueExpr $
    RawSql.fromString "MINVALUE "
      <> RawSql.int64DecLiteral n

noMinValue :: MinValueExpr
noMinValue =
  MinValueExpr . RawSql.fromString $ "NO MINVALUE"

newtype MaxValueExpr
  = MaxValueExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

maxValue :: Int64 -> MaxValueExpr
maxValue n =
  MaxValueExpr $
    RawSql.fromString "MAXVALUE "
      <> RawSql.int64DecLiteral n

noMaxValue :: MaxValueExpr
noMaxValue =
  MaxValueExpr . RawSql.fromString $ "NO MAXVALUE"

newtype StartWithExpr
  = StartWithExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

startWith :: Int64 -> StartWithExpr
startWith n =
  StartWithExpr $
    RawSql.fromString "START WITH "
      <> RawSql.int64DecLiteral n

newtype CacheExpr
  = CacheExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

cache :: Int64 -> CacheExpr
cache n =
  CacheExpr $
    RawSql.fromString "CACHE "
      <> RawSql.int64DecLiteral n

newtype CycleExpr
  = CycleExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

cycle :: CycleExpr
cycle = CycleExpr $ RawSql.fromString "CYCLE"

noCycle :: CycleExpr
noCycle = CycleExpr $ RawSql.fromString "NO CYCLE"

cycleIfTrue :: Bool -> CycleExpr
cycleIfTrue cycleFlag =
  if cycleFlag
    then cycle
    else noCycle

newtype DropSequenceExpr
  = DropSequenceExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

dropSequenceExpr :: Maybe IfExists -> Qualified SequenceName -> DropSequenceExpr
dropSequenceExpr maybeIfExists sequenceName =
  DropSequenceExpr $
    RawSql.intercalate
      RawSql.space
      ( catMaybes
          [ Just (RawSql.fromString "DROP SEQUENCE")
          , fmap RawSql.toRawSql maybeIfExists
          , Just (RawSql.toRawSql sequenceName)
          ]
      )

nextVal :: Qualified SequenceName -> ValueExpression
nextVal sequenceName =
  functionCall
    nextValFunction
    [valueExpression . SqlValue.fromRawBytes . RawSql.toExampleBytes $ sequenceName]

nextValFunction :: FunctionName
nextValFunction =
  functionName "nextval"

currVal :: Qualified SequenceName -> ValueExpression
currVal sequenceName =
  functionCall
    currValFunction
    [valueExpression . SqlValue.fromRawBytes . RawSql.toExampleBytes $ sequenceName]

currValFunction :: FunctionName
currValFunction =
  functionName "currval"

setVal :: Qualified SequenceName -> Int64 -> ValueExpression
setVal sequenceName newValue =
  functionCall
    setValFunction
    [ valueExpression . SqlValue.fromRawBytes . RawSql.toExampleBytes $ sequenceName
    , valueExpression . SqlValue.fromInt64 $ newValue
    ]

setValFunction :: FunctionName
setValFunction =
  functionName "setval"
