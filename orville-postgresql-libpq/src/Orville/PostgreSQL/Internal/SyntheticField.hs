{-# LANGUAGE OverloadedStrings #-}

module Orville.PostgreSQL.Internal.SyntheticField
  ( SyntheticField,
    syntheticFieldExpression,
    syntheticFieldAlias,
    syntheticFieldValueFromSqlValue,
    syntheticField,
    nullableSyntheticField,
    prefixSyntheticField,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import Orville.PostgreSQL.Internal.FieldDefinition (FieldName, byteStringToFieldName, fieldNameToByteString, stringToFieldName)
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

{- |
  A 'SyntheticField' can be used to evaluate a SQL expression based on the
  columns of a table when records are selected from the database. Synthetic
  fields are inherently read-only.
-}
data SyntheticField a = SyntheticField
  { _syntheticFieldExpression :: Expr.ValueExpression
  , _syntheticFieldAlias :: FieldName
  , _syntheticFieldValueFromSqlValue :: SqlValue.SqlValue -> Either String a
  }

{- |
  Returns the SQL expression that should be in with select statements to
  calculated the sythetic field.
-}
syntheticFieldExpression :: SyntheticField a -> Expr.ValueExpression
syntheticFieldExpression =
  _syntheticFieldExpression

{- |
  Returns the alias that should be used in select statements to name the
  the synthetic field.
-}
syntheticFieldAlias :: SyntheticField a -> FieldName
syntheticFieldAlias =
  _syntheticFieldAlias

{- |
  Decodes a calculated value selected from the database to its expected
  Haskell type. Returns a 'Left' with an error message if the decoding fails.
-}
syntheticFieldValueFromSqlValue :: SyntheticField a -> SqlValue.SqlValue -> Either String a
syntheticFieldValueFromSqlValue =
  _syntheticFieldValueFromSqlValue

{- |
  Constructs a 'SyntheticField' that will select a SQL expression using
  the given alias.
-}
syntheticField ::
  -- | The SQL expression to be selected
  Expr.ValueExpression ->
  -- | The alias to be used to name the calculation in SQL experios
  String ->
  -- | A function to decode the expression result from a 'SqlValue.SqlValue'
  (SqlValue.SqlValue -> Either String a) ->
  SyntheticField a
syntheticField expression alias fromSqlValue =
  SyntheticField
    { _syntheticFieldExpression = expression
    , _syntheticFieldAlias = stringToFieldName alias
    , _syntheticFieldValueFromSqlValue = fromSqlValue
    }

{- |
  Modifies a 'SyntheticField' to allow it to decode @NULL@ values.
-}
nullableSyntheticField :: SyntheticField a -> SyntheticField (Maybe a)
nullableSyntheticField synthField =
  synthField
    { _syntheticFieldValueFromSqlValue = \sqlValue ->
        if SqlValue.isSqlNull sqlValue
          then Right Nothing
          else Just <$> syntheticFieldValueFromSqlValue synthField sqlValue
    }

{- |
  Adds a prefix, followed by an underscore, to the alias used to name the
  synthetic field.
-}
prefixSyntheticField ::
  String ->
  SyntheticField a ->
  SyntheticField a
prefixSyntheticField prefix synthField =
  synthField
    { _syntheticFieldAlias = byteStringToFieldName (B8.pack prefix <> "_" <> fieldNameToByteString (syntheticFieldAlias synthField))
    }
