{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.SequenceDefinition
  ( CreateSequenceExpr
  , createSequenceExpr
  , AlterSequenceExpr
  , alterSequenceExpr
  , IncrementByExpr
  , incrementBy
  , MinValueExpr
  , minValue
  , noMinValue
  , MaxValueExpr
  , maxValue
  , noMaxValue
  , StartWithExpr
  , startWith
  , CacheExpr
  , cache
  , CycleExpr
  , cycle
  , noCycle
  , cycleIfTrue
  , DropSequenceExpr
  , dropSequenceExpr
  , nextVal
  , nextValFunction
  , currVal
  , currValFunction
  , setVal
  , setValFunction
  )
where

-- to avoid conflict with cycle
import Prelude (Bool, Maybe (Just), fmap, ($), (.), (<>))

import Data.Int (Int64)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.Name (FunctionName, Qualified, SequenceName, functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, functionCall, valueExpression)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

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

{- |
Type to represent a @CREATE SEQUENCE@ statement. E.G.

> CREATE SEQUENCE foo INCREMENT 2

'CreateSequenceExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype CreateSequenceExpr
  = CreateSequenceExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'CreateSequenceExpr' with the given sequence options.

  @since 1.0.0.0
-}
createSequenceExpr ::
  -- | The name to be used for the sequence.
  Qualified SequenceName ->
  -- | An optional @INCREMENT@ expression.
  Maybe IncrementByExpr ->
  -- | An optional @MINVALUE@ expression.
  Maybe MinValueExpr ->
  -- | An optional @MAXVALUE@ expression.
  Maybe MaxValueExpr ->
  -- | An optional @START WITH@ expression.
  Maybe StartWithExpr ->
  -- | An optional @CACHE@ expression.
  Maybe CacheExpr ->
  -- | An optional @CYCLE@ expression.
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

{- |
Type to represent a @CREATE SEQUENCE@ statement. E.G.

> ALTER SEQUENCE foo START WITH 0

'AlterSequenceExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype AlterSequenceExpr
  = AlterSequenceExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs an 'AlterSequenceExpr' with the given sequence options.

  @since 1.0.0.0
-}
alterSequenceExpr ::
  -- | The name of the sequence to alter
  Qualified SequenceName ->
  -- | An optional @INCREMENT@ expression
  Maybe IncrementByExpr ->
  -- | An optional @MINVALUE@ expression
  Maybe MinValueExpr ->
  -- | An optional @MAXVALUE@ expression
  Maybe MaxValueExpr ->
  -- | An optional @START WITH@ expression
  Maybe StartWithExpr ->
  -- | An optional @CACHE@ expression
  Maybe CacheExpr ->
  -- | An optional @CYCLE@ expression
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

{- |
Type to represent an @INCREMENT BY@ expression for sequences. E.G.

> INCREMENT BY 0

'IncrementByExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype IncrementByExpr
  = IncrementByExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs an 'IncrementByExpr' that will make the sequence increment by
  the given value.

  @since 1.0.0.0
-}
incrementBy :: Int64 -> IncrementByExpr
incrementBy n =
  IncrementByExpr $
    RawSql.fromString "INCREMENT BY "
      <> RawSql.int64DecLiteral n

{- |
Type to represent a @MINVALUE@ expression for sequences. E.G.

> MINVALUE 0

'MinValueExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype MinValueExpr
  = MinValueExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'MinValueExpr' which gives the sequence the specified minimum
  value.

  @since 1.0.0.0
-}
minValue :: Int64 -> MinValueExpr
minValue n =
  MinValueExpr $
    RawSql.fromString "MINVALUE "
      <> RawSql.int64DecLiteral n

{- |
  Constructs a 'MinValueExpr' which gives the sequence no minimum value (i.e.
  @NO MINVALUE@).

  @since 1.0.0.0
-}
noMinValue :: MinValueExpr
noMinValue =
  MinValueExpr . RawSql.fromString $ "NO MINVALUE"

{- |
Type to represent a @MAXVALUE@ expression for sequences. E.G.

> MAXVALUE 1000000

'MaxValueExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype MaxValueExpr
  = MaxValueExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'MaxValueExpr' which gives the sequence the specified maximum
  value.

  @since 1.0.0.0
-}
maxValue :: Int64 -> MaxValueExpr
maxValue n =
  MaxValueExpr $
    RawSql.fromString "MAXVALUE "
      <> RawSql.int64DecLiteral n

{- |
  Constructs a 'MaxValueExpr' which gives the sequence no maximum value (i.e.
  @NO MAXVALUE@).

  @since 1.0.0.0
-}
noMaxValue :: MaxValueExpr
noMaxValue =
  MaxValueExpr . RawSql.fromString $ "NO MAXVALUE"

{- |
Type to represent a @START WITH@ expression for sequences. E.G.

> START WITH 1

'StartWithExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype StartWithExpr
  = StartWithExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'StartWithExpr' which gives the sequence the specified start
  value.

  @since 1.0.0.0
-}
startWith :: Int64 -> StartWithExpr
startWith n =
  StartWithExpr $
    RawSql.fromString "START WITH "
      <> RawSql.int64DecLiteral n

{- |
Type to represent a @CACHE@ expression for sequences. E.G.

> CACHE 16

'CacheExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype CacheExpr
  = CacheExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'CacheExpr' that will make the sequence pre-allocate the
  specified number of sequence values.

  @since 1.0.0.0
-}
cache :: Int64 -> CacheExpr
cache n =
  CacheExpr $
    RawSql.fromString "CACHE "
      <> RawSql.int64DecLiteral n

{- |
Type to represent a @CYCLE@ expression for sequences. E.G.

> CYCLE

or

> NO CYCLE

'CycleExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype CycleExpr
  = CycleExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'CycleExpr' that indicates that the sequence should cycle.

  @since 1.0.0.0
-}
cycle :: CycleExpr
cycle = CycleExpr $ RawSql.fromString "CYCLE"

{- |
  Constructs a 'CycleExpr' that indicates that the sequence should not cycle.

  @since 1.0.0.0
-}
noCycle :: CycleExpr
noCycle = CycleExpr $ RawSql.fromString "NO CYCLE"

{- |
  Constructs a 'CycleExpr' that will cause the sequence to cycle if the flag
  passed is @True@.

  @since 1.0.0.0
-}
cycleIfTrue :: Bool -> CycleExpr
cycleIfTrue cycleFlag =
  if cycleFlag
    then cycle
    else noCycle

{- |
Type to represent a @DROP SEQUENCE@ statement. E.G.

> DROP SEQUENCE foo

'DropSequenceExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype DropSequenceExpr
  = DropSequenceExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'DropSequenceExpr' that will drop sequence with the given name.
  You may specify an 'IfExists' argument if you want to include an @IF EXISTS@
  condition in the statement.

  @since 1.0.0.0
-}
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

{- |
  Constructs a 'ValueExpression' that will use the @nextval@ PostgreSQL
  function to get the next value from the given sequence. If you're trying to
  construct your own @SELECT@ to get the value of the sequence, you can use the
  constructed 'ValueExpression' with 'Orville.PostgreSQL.Expr.deriveColumnAs'
  to build the item to select.

  @since 1.0.0.0
-}
nextVal :: Qualified SequenceName -> ValueExpression
nextVal sequenceName =
  functionCall
    nextValFunction
    [valueExpression . SqlValue.fromRawBytes . RawSql.toExampleBytes $ sequenceName]

{- |
  The @nextval@ PostgreSQL function.

  @since 1.0.0.0
-}
nextValFunction :: FunctionName
nextValFunction =
  functionName "nextval"

{- |
  Constructs a 'ValueExpression' that will use the @currval@ PostgreSQL
  function to get the current value from the given sequence. If you're trying to
  construct your own @SELECT@ to get the value of the sequence, you can use the
  constructed 'ValueExpression' with 'Orville.PostgreSQL.Expr.deriveColumnAs'
  to build the item to select.

  @since 1.0.0.0
-}
currVal :: Qualified SequenceName -> ValueExpression
currVal sequenceName =
  functionCall
    currValFunction
    [valueExpression . SqlValue.fromRawBytes . RawSql.toExampleBytes $ sequenceName]

{- |
  The @currval@ PostgreSQL function.

  @since 1.0.0.0
-}
currValFunction :: FunctionName
currValFunction =
  functionName "currval"

{- |
  Constructs a 'ValueExpression' that will use the @setval@ PostgreSQL function
  to set the value from the given sequence. If you're trying to construct your
  own @SELECT@ to set the value of the sequence, you can use the constructed
  'ValueExpression' with 'Orville.PostgreSQL.Expr.deriveColumnAs' to build the
  item to select.

  @since 1.0.0.0
-}
setVal :: Qualified SequenceName -> Int64 -> ValueExpression
setVal sequenceName newValue =
  functionCall
    setValFunction
    [ valueExpression . SqlValue.fromRawBytes . RawSql.toExampleBytes $ sequenceName
    , valueExpression . SqlValue.fromInt64 $ newValue
    ]

{- |
  The @setval@ PostgreSQL function.

  @since 1.0.0.0
-}
setValFunction :: FunctionName
setValFunction =
  functionName "setval"
