{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

Functionality for loading and using the
[pg_trgm](https://www.postgresql.org/docs/current/pgtrgm.html) extension. While this extension is
supplied by default with PostgreSQL, it is entirely up to users to ensure access to the extension.

@since 1.1.0.0
-}
module Orville.PostgreSQL.Extension.PgTrgm
  ( trigramSimilaritySyntheticField
  , trigramWordSimilaritySyntheticField
  , trigramStrictWordSimilaritySyntheticField
  , mkNamedTrigramGinIndexDefinition
  , mkNamedTrigramGistIndexDefinition
  , trigramSimilarity
  , trigramWordSimilarity
  , trigramStrictWordSimilarity
  ) where

import qualified Data.List.NonEmpty as NEL

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.IndexDefinition as IndexDefinition
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- | Create a named GIN index over the given fields for fast text searching.

@since 1.1.0.0
-}
mkNamedTrigramGinIndexDefinition ::
  -- | The name of the index to be created.
  String ->
  -- | Field names to create the index over.
  NEL.NonEmpty Marshall.FieldName ->
  IndexDefinition.IndexDefinition
mkNamedTrigramGinIndexDefinition name =
  IndexDefinition.mkNamedIndexDefinition Expr.NonUniqueIndex name . RawSql.unsafeFromRawSql . trigramGinIndexFieldsExpr

trigramGinIndexFieldsExpr :: NEL.NonEmpty Marshall.FieldName -> RawSql.RawSql
trigramGinIndexFieldsExpr fields =
  let
    fieldExpr :: Marshall.FieldName -> RawSql.RawSql
    fieldExpr name =
      RawSql.toRawSql (Marshall.fieldNameToColumnName name)
        <> RawSql.space
        <> RawSql.fromString "gin_trgm_ops"
  in
    RawSql.fromString "USING GIN "
      <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace (fmap fieldExpr fields))

{- | Create a named GIST index for fast text searching.  The index is created, over the fields each of
  with an optional override of the index parameter "siglen". See [pg_trgm index
  docs](https://www.postgresql.org/docs/current/pgtrgm.html#PGTRGM-INDEX) for more information.

@since 1.1.0.0
-}
mkNamedTrigramGistIndexDefinition ::
  -- | The name of the index to be created.
  String ->
  -- | Pairs of field name and optionally a value of the siglen parameter, to create the index over.
  NEL.NonEmpty (Marshall.FieldName, Maybe Int) ->
  IndexDefinition.IndexDefinition
mkNamedTrigramGistIndexDefinition name =
  IndexDefinition.mkNamedIndexDefinition Expr.NonUniqueIndex name . RawSql.unsafeFromRawSql . trigramGistIndexFieldsExpr

trigramGistIndexFieldsExpr ::
  NEL.NonEmpty (Marshall.FieldName, Maybe Int) ->
  RawSql.RawSql
trigramGistIndexFieldsExpr fields =
  let
    fieldExpr :: (Marshall.FieldName, Maybe Int) -> RawSql.RawSql
    fieldExpr (name, mbSiglen) =
      RawSql.toRawSql (Marshall.fieldNameToColumnName name)
        <> RawSql.space
        <> RawSql.fromString "gist_trgm_ops"
        <> maybe
          mempty
          (\len -> RawSql.parenthesized (RawSql.fromString "siglen=" <> RawSql.intDecLiteral len))
          mbSiglen
  in
    RawSql.fromString "USING GIST "
      <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace (fmap fieldExpr fields))

{- | Create a synthetic field using the similarity function provided by pg_trgm.

@since 1.1.0.0
-}
trigramSimilaritySyntheticField ::
  -- | The column to be used in the similarity comparison
  Expr.ColumnName ->
  -- | The value to be compared against.
  Expr.ValueExpression ->
  -- | The alias to be used to name the similarity result.
  String ->
  -- | A field with the resulting similarity score between the column value and the comparison.
  Marshall.SyntheticField Double
trigramSimilaritySyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (trigramSimilarity (Expr.columnReference colname) compareVal)
    fieldAlias
    SqlValue.toDouble

{- | Create a synthetic field using the word_similarity function provided by pg_trgm.

@since 1.1.0.0
-}
trigramWordSimilaritySyntheticField ::
  -- | The column to be used in the word_similarity comparison
  Expr.ColumnName ->
  -- | The value to be compared against.
  Expr.ValueExpression ->
  -- | The alias to be used to name the word_similarity result.
  String ->
  -- | A field with the resulting word_similarity score between the column value and the comparison.
  Marshall.SyntheticField Double
trigramWordSimilaritySyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (trigramWordSimilarity (Expr.columnReference colname) compareVal)
    fieldAlias
    SqlValue.toDouble

{- | Create a synthetic field using the strict_word_similarity function provided by pg_trgm.

@since 1.1.0.0
-}
trigramStrictWordSimilaritySyntheticField ::
  -- | The column to be used in the strict_word_similarity comparison
  Expr.ColumnName ->
  -- | The value to be compared against.
  Expr.ValueExpression ->
  -- | The alias to be used to name the strict_word_similarity result.
  String ->
  -- | A field with the resulting strict_word_similarity score between the column value and the comparison.
  Marshall.SyntheticField Double
trigramStrictWordSimilaritySyntheticField colname compareVal fieldAlias =
  Marshall.syntheticField
    (trigramStrictWordSimilarity (Expr.columnReference colname) compareVal)
    fieldAlias
    SqlValue.toDouble

{- | Call the similarity function provided by pg_trgm, comparing the pair of values.

@since 1.1.0.0
-}
trigramSimilarity ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
trigramSimilarity firstVal secondVal =
  Expr.functionCall
    (Expr.functionName "similarity")
    [firstVal, secondVal]

{- | Call the word_similarity function provided by pg_trgm, comparing the pair of values.

@since 1.1.0.0
-}
trigramWordSimilarity ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
trigramWordSimilarity firstVal secondVal =
  Expr.functionCall
    (Expr.functionName "word_similarity")
    [firstVal, secondVal]

{- | Call the strict_word_similarity function provided by pg_trgm, comparing the pair of values.

@since 1.1.0.0
-}
trigramStrictWordSimilarity ::
  Expr.ValueExpression ->
  Expr.ValueExpression ->
  Expr.ValueExpression
trigramStrictWordSimilarity firstVal secondVal =
  Expr.functionCall
    (Expr.functionName "strict_word_similarity")
    [firstVal, secondVal]
