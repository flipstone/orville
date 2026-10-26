{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.RelationOption
  ( RelationOption
  , relationOption
  , RelationOptionName
  , relationOptionName
  , toastRelationOptionName
  , RelationOptionValue
  , relationOptionBool
  , relationOptionInt
  , relationOptionText
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent one option in the parenthesized option list that
PostgreSQL accepts in several places under different names: the view
options of @CREATE VIEW ... WITH (...)@, the storage parameters of
@CREATE MATERIALIZED VIEW ... WITH (...)@ and @ALTER ... SET (...)@, and the
attribute options of @ALTER ... ALTER COLUMN ... SET (...)@. E.G.

> "fillfactor" = 70

'RelationOption' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype RelationOption
  = RelationOption RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'RelationOption' that sets the named option to the given value, or names the option
without a value, which PostgreSQL treats as setting a boolean option to @true@.

@since 1.2.0.0
-}
relationOption ::
  RelationOptionName ->
  Maybe RelationOptionValue ->
  RelationOption
relationOption name mbValue =
  RelationOption $
    case mbValue of
      Nothing ->
        RawSql.toRawSql name
      Just value ->
        RawSql.toRawSql name <> RawSql.fromString " = " <> RawSql.toRawSql value

{- | Type to represent the name of a 'RelationOption'. E.G.

> "fillfactor"

'RelationOptionName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype RelationOptionName
  = RelationOptionName RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'RelationOptionName' from a 'String', which is quoted as an identifier in the
generated SQL.

@since 1.2.0.0
-}
relationOptionName :: String -> RelationOptionName
relationOptionName =
  RelationOptionName . RawSql.toRawSql . identifier

{- | Constructs a 'RelationOptionName' for an option of the TOAST table of a relation, which
PostgreSQL names with a @toast.@ prefix. E.G.

> "toast"."autovacuum_enabled"

@since 1.2.0.0
-}
toastRelationOptionName :: String -> RelationOptionName
toastRelationOptionName name =
  RelationOptionName $
    RawSql.toRawSql (identifier "toast")
      <> RawSql.dot
      <> RawSql.toRawSql (identifier name)

{- | Type to represent the value of a 'RelationOption'. E.G.

> 70

'RelationOptionValue' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype RelationOptionValue
  = RelationOptionValue RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | A boolean option value, rendered as @true@ or @false@.

@since 1.2.0.0
-}
relationOptionBool :: Bool -> RelationOptionValue
relationOptionBool flag =
  RelationOptionValue . RawSql.fromString $
    if flag
      then "true"
      else "false"

{- | An integer option value.

@since 1.2.0.0
-}
relationOptionInt :: Int -> RelationOptionValue
relationOptionInt =
  RelationOptionValue . RawSql.intDecLiteral

{- | A text option value, rendered as a quoted string literal.

@since 1.2.0.0
-}
relationOptionText :: T.Text -> RelationOptionValue
relationOptionText =
  RelationOptionValue . RawSql.stringLiteral . TEnc.encodeUtf8
