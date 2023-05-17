{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.Time
  ( now
  , makeInterval
  , IntervalArgument
  , years
  , months
  , weeks
  , days
  , hours
  , minutes
  , seconds
  )
where

import Orville.PostgreSQL.Expr.Name (functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ParameterName, ValueExpression, functionCall, functionCallNamedParams)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
  The value of the current time as returned by the PostgreSQL function @now()@.

  @since 0.10.0.0
-}
now :: ValueExpression
now = functionCall (functionName "now") []

{- |
  Constructs a 'ValueExpression' whose value in PostgreSQL is the result of
  calling @make_interval@ with the specified time intervals passed as named
  arguments.

  @since 0.10.0.0
-}
makeInterval :: [(IntervalArgument, ValueExpression)] -> ValueExpression
makeInterval args =
  functionCallNamedParams
    (functionName "make_interval")
    (fmap (\(IntervalArgument paramName, value) -> (paramName, value)) args)

{- |
Type to represent the name of a time interval argument to the PostgreSQL
@make_interval@ function. E.G.

> years

'IntervalArgument' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype IntervalArgument
  = IntervalArgument ParameterName
  deriving (RawSql.SqlExpression)

{- |
  Constructs an arbitrary 'IntervalArgument' with whatever name you specify. It
  is up to you to ensure that name a valid argument name for @make_interval@.

  @since 0.10.0.0
-}
unsafeIntervalArg :: String -> IntervalArgument
unsafeIntervalArg =
  IntervalArgument . RawSql.unsafeSqlExpression

{- |
  The @years@ argument to @make_interval@.

  @since 0.10.0.0
-}
years :: IntervalArgument
years = unsafeIntervalArg "years"

{- |
  The @months@ argument to @make_interval@.

  @since 0.10.0.0
-}
months :: IntervalArgument
months = unsafeIntervalArg "months"

{- |
  The @weeks@ argument to @make_interval@.

  @since 0.10.0.0
-}
weeks :: IntervalArgument
weeks = unsafeIntervalArg "weeks"

{- |
  The @days@ argument to @make_interval@.

  @since 0.10.0.0
-}
days :: IntervalArgument
days = unsafeIntervalArg "days"

{- |
  The @hours@ argument to @make_interval@.

  @since 0.10.0.0
-}
hours :: IntervalArgument
hours = unsafeIntervalArg "hours"

{- |
  The @mins@ argument to @make_interval@.

  @since 0.10.0.0
-}
minutes :: IntervalArgument
minutes = unsafeIntervalArg "mins"

{- |
  The @secs@ argument to @make_interval@.

  @since 0.10.0.0
-}
seconds :: IntervalArgument
seconds = unsafeIntervalArg "secs"
