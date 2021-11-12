{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.DefaultValue
  ( DefaultValue,
    integerDefault,
    smallIntegerDefault,
    bigIntegerDefault,
    integralDefault,
    doubleDefault,
    booleanDefault,
    textDefault,
    dateDefault,
    currentDateDefault,
    utcTimestampDefault,
    currentUTCTimestampDefault,
    localTimestampDefault,
    currentLocalTimestampDefault,
    coerceDefaultValue,
    defaultValueExpression,
    rawSqlDefault,
  )
where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time as Time

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.PgTime as PgTime
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

{- |
  A 'DefaultValue' is a SQL expression that can be attached to a
  field definition to give a default value for a column at the database level.
  The default value will be used if an insert is done and the column is not
  provided.

  This is useful if you want to add a new column to a table that is already
  in production without breaking a previous version of your application that
  is running (e.g. during a zero-down-time deployment) and without needing to
  make the new column nullable. Default values can also be used to create
  database-assigned values such as using 'now()' to set a 'created_at' column
  on a row automatically in the database.
-}
newtype DefaultValue a
  = DefaultValue Expr.ValueExpression

{- |
  Builds a default value for any 'Integral' type @n@ by converting it an
  'Integer'.
-}
integralDefault :: Integral n => n -> DefaultValue n
integralDefault n =
  let decimalBytes =
        LBS.toStrict
          . BSB.toLazyByteString
          . BSB.integerDec
          . toInteger
          $ n
   in if n < 0
        then
          DefaultValue . RawSql.unsafeFromRawSql $
            RawSql.stringLiteral decimalBytes
              <> RawSql.fromString "::integer"
        else DefaultValue . RawSql.unsafeFromRawSql . RawSql.fromBytes $ decimalBytes

{- |
  Builds a default value from an 'Int16' for use with small integer fields.

  This is a specialization of 'integerDefault'.
-}
smallIntegerDefault :: Int16 -> DefaultValue Int16
smallIntegerDefault = integralDefault

{- |
  Builds a default value from an 'Int32' for use with integer fields.

  This is a specialization of 'integerDefault'.
-}
integerDefault :: Int32 -> DefaultValue Int32
integerDefault = integralDefault

{- |
  Builds a default value from an 'Int16' for use with big integer fields.

  This is a specialization of 'integerDefault'.
-}
bigIntegerDefault :: Int64 -> DefaultValue Int64
bigIntegerDefault = integralDefault

{- |
  Builds a default value from a 'Double' field with double fields.
-}
doubleDefault :: Double -> DefaultValue Double
doubleDefault d =
  let decimalBytes =
        LBS.toStrict
          . BSB.toLazyByteString
          . BSB.doubleDec
          $ d
   in if d < 0
        then
          DefaultValue . RawSql.unsafeFromRawSql $
            RawSql.stringLiteral decimalBytes
              <> RawSql.fromString "::numeric"
        else DefaultValue . RawSql.unsafeFromRawSql . RawSql.fromBytes $ decimalBytes

{- |
  Builds a default value from a 'Bool', for use with boolean fields.
-}
booleanDefault :: Bool -> DefaultValue Bool
booleanDefault bool =
  let pgString =
        case bool of
          True -> "true"
          False -> "false"
   in DefaultValue . RawSql.unsafeFromRawSql . RawSql.fromString $ pgString

{- |
  Builds a default value from a 'T.Text', for use with unbounded, bounded
  and fixed-length text fields.
-}
textDefault :: T.Text -> DefaultValue T.Text
textDefault text =
  DefaultValue . RawSql.unsafeFromRawSql $
    RawSql.stringLiteral (TextEnc.encodeUtf8 text)
      <> RawSql.fromString "::text"

{- |
  Builds a default value from a 'Time.Day' for use with date fields.
-}
dateDefault :: Time.Day -> DefaultValue Time.Day
dateDefault day =
  let pgText =
        PgTime.dayToPostgreSQL day
   in DefaultValue . RawSql.unsafeFromRawSql $
        RawSql.stringLiteral pgText
          <> RawSql.fromString "::date"

{- |
  Builds a default value that will default to the current date (i.e. the
  date at which the database populates the default value on a given row).

  For use with date fields.
-}
currentDateDefault :: DefaultValue Time.Day
currentDateDefault =
  DefaultValue
    . RawSql.unsafeFromRawSql
    . RawSql.fromString
    $ "('now'::text)::date"

{- |
  Builds a default value from a 'Time.UTCTime' for use with utc timestamp fields.
-}
utcTimestampDefault :: Time.UTCTime -> DefaultValue Time.UTCTime
utcTimestampDefault utcTime =
  let pgText =
        PgTime.utcTimeToPostgreSQL utcTime
   in DefaultValue . RawSql.unsafeFromRawSql $
        RawSql.stringLiteral pgText
          <> RawSql.fromString "::timestamp with time zone"

{- |
  Builds a default value that will default to the current utc time (i.e. the
  time at which the database populates the default value on a given row).

  For use with utc timestamp fields.
-}
currentUTCTimestampDefault :: DefaultValue Time.UTCTime
currentUTCTimestampDefault =
  DefaultValue
    . RawSql.unsafeFromRawSql
    . RawSql.fromString
    $ "now()"

{- |
  Builds a default value from a 'Time.LocalTime' for use with local timestamp fields.
-}
localTimestampDefault :: Time.LocalTime -> DefaultValue Time.LocalTime
localTimestampDefault localTime =
  let pgText =
        PgTime.localTimeToPostgreSQL localTime
   in DefaultValue
        . RawSql.unsafeFromRawSql
        $ RawSql.stringLiteral pgText
          <> RawSql.fromString "::timestamp without time zone"

{- |
  Builds a default value that will default to the current local time (i.e. the
  time at which the database populates the default value on a given row).

  Note: "local" time here will be determined by the database itself, subject to
  whatever timezone offset has been configured in its settings.

  For use with local timestamp fields.
-}
currentLocalTimestampDefault :: DefaultValue Time.LocalTime
currentLocalTimestampDefault =
  DefaultValue
    . RawSql.unsafeFromRawSql
    . RawSql.fromString
    $ "('now'::text)::timestamp without time zone"

{- |
  Coerce's a 'DefaultValue' so that it can be used with field definitions of
  a different Haskell type. The coercion will always succeed, and is safe as
  far as Haskell itself it concerned. As long as the 'DefaultValue' is used
  with a column whose database type is the same as the one the 'DefaultValue'
  was originally intended for, everything will work as expected.
-}
coerceDefaultValue :: DefaultValue a -> DefaultValue b
coerceDefaultValue (DefaultValue expression) =
  DefaultValue expression

{- |
  Returns database value expression for the default value
-}
defaultValueExpression :: DefaultValue a -> Expr.ValueExpression
defaultValueExpression (DefaultValue expression) =
  expression

{- |
  Constructs a default value from a 'ValueExpression'. You can use this
  to construct default values for any SQL expression that Orville does not
  support directly.

  Note: If you are using auto migrations, the 'Expr.ValueExpression' that you
  pass here must match what is returned by the PostgreSQL @pg_get_expr@
  function. @pg_get_expr@ decompiles the compiled version of the default
  experssion back to source text, sometimes in non-obvious ways. Orville's auto
  migration compares expression given in the field definition with the
  decompiled expression from the database to determine whether the default
  value needs to be updated in the schema or not.  If the expression given by a
  'DefaultValue' is logically equivalent but does not match the decompiled
  form, auto migration will continue to execute SQL statements to update the
  schema even when it does not need to.
-}
rawSqlDefault :: Expr.ValueExpression -> DefaultValue a
rawSqlDefault =
  DefaultValue
