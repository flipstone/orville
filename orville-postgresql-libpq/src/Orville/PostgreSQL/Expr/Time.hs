{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.Time
  ( now,
    makeInterval,
    IntervalArgument,
    years,
    months,
    weeks,
    days,
    hours,
    minutes,
    seconds,
  )
where

import Orville.PostgreSQL.Expr.Name (functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ParameterName, ValueExpression, functionCall, functionCallNamedParams)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

now :: ValueExpression
now = functionCall (functionName "now") []

makeInterval :: [(IntervalArgument, ValueExpression)] -> ValueExpression
makeInterval args =
  functionCallNamedParams
    (functionName "make_interval")
    (fmap (\(IntervalArgument paramName, value) -> (paramName, value)) args)

newtype IntervalArgument
  = IntervalArgument ParameterName
  deriving (RawSql.SqlExpression)

unsafeIntervalArg :: String -> IntervalArgument
unsafeIntervalArg =
  IntervalArgument . RawSql.unsafeFromRawSql . RawSql.fromString

years :: IntervalArgument
years = unsafeIntervalArg "years"

months :: IntervalArgument
months = unsafeIntervalArg "months"

weeks :: IntervalArgument
weeks = unsafeIntervalArg "weeks"

days :: IntervalArgument
days = unsafeIntervalArg "days"

hours :: IntervalArgument
hours = unsafeIntervalArg "hours"

minutes :: IntervalArgument
minutes = unsafeIntervalArg "mins"

seconds :: IntervalArgument
seconds = unsafeIntervalArg "secs"
