{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024-2025
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.TextSearch
  ( TSVector
  , tsVectorToValueExpression
  , TSQuery
  , tsQueryToValueExpression
  , TSWeight
  , tsMatch
  , tsMatchTSQueryTSVector
  , tsVectorConcat
  , toTSVector
  , toTSQuery
  , plainToTSQuery
  , toTSRank
  , setTSWeight
  , tsWeightToValueExpression
  , tsWeightA
  , tsWeightB
  , tsWeightC
  , tsWeightD
  , RegConfig
  , arabicRegConfig
  , danishRegConfig
  , dutchRegConfig
  , englishRegConfig
  , finnishRegConfig
  , frenchRegConfig
  , germanRegConfig
  , greekRegConfig
  , hungarianRegConfig
  , indonesianRegConfig
  , irishRegConfig
  , italianRegConfig
  , lithuanianRegConfig
  , nepaliRegConfig
  , norwegianRegConfig
  , portugueseRegConfig
  , romanianRegConfig
  , russianRegConfig
  , simpleRegConfig
  , spanishRegConfig
  , swedishRegConfig
  , tamilRegConfig
  , turkishRegConfig
  ) where

import Orville.PostgreSQL.Expr.BinaryOperator (BinaryOperator, binaryOpExpression, binaryOperator)
import Orville.PostgreSQL.Expr.Name (FunctionName, functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, functionCall)
import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a PostgreSQL @tsvector@.

'TSVector' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

 @since 1.1.0.0
-}
newtype TSVector
  = TSVector RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Type to represent a PostgreSQL @tsquery@.

'TSQuery' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

 @since 1.1.0.0
-}
newtype TSQuery
  = TSQuery RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | A type to represent the weight to be given to elements of a 'TSVector'

'TSWeight' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

  @since 1.1.0.0
-}
newtype TSWeight
  = TSWeight RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

tsMatchOp :: BinaryOperator
tsMatchOp = binaryOperator "@@"

tsVectorConcatOp :: BinaryOperator
tsVectorConcatOp = binaryOperator "||"

{- | Matches a 'TSVector' against a 'TSQuery'. This function is an alias for the '@@' operator.

@since 1.1.0.0
-}
tsMatch :: TSVector -> TSQuery -> BooleanExpr
tsMatch tsVector tsQuery =
  binaryOpExpression
    tsMatchOp
    (tsVectorToValueExpression tsVector)
    (tsQueryToValueExpression tsQuery)

{- | Matches a 'TSQuery' against a 'TSVector'. This function is an alias for the '@@' operator.
    Filped version of 'tsMatch'

@since 1.1.0.0
-}
tsMatchTSQueryTSVector :: TSQuery -> TSVector -> BooleanExpr
tsMatchTSQueryTSVector = flip tsMatch

{- | Concatenates two 'TSVector' values.

@since 1.1.0.0
-}
tsVectorConcat :: TSVector -> TSVector -> TSVector
tsVectorConcat tsVector1 tsVector2 =
  binaryOpExpression
    tsVectorConcatOp
    (tsVectorToValueExpression tsVector1)
    (tsVectorToValueExpression tsVector2)

{- | A constant representing the "A" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightA :: TSWeight
tsWeightA = TSWeight $ RawSql.fromString "\'A\'"

{- | A constant representing the "B" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightB :: TSWeight
tsWeightB = TSWeight $ RawSql.fromString "\'B\'"

{- | A constant representing the "C" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightC :: TSWeight
tsWeightC = TSWeight $ RawSql.fromString "\'C\'"

{- | A constant representing the "D" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightD :: TSWeight
tsWeightD = TSWeight $ RawSql.fromString "\'D\'"

toTSVectorFunction :: FunctionName
toTSVectorFunction = functionName "to_tsvector"

toTSQueryFunction :: FunctionName
toTSQueryFunction = functionName "to_tsquery"

toTSRankFunction :: FunctionName
toTSRankFunction = functionName "ts_rank"

setTSWeightFunction :: FunctionName
setTSWeightFunction = functionName "setweight"

plaintoTSQueryFunction :: FunctionName
plaintoTSQueryFunction = functionName "plainto_tsquery"

{- | Converts a 'RegConfig' to a 'ValueExpression'.

@since 1.1.0.0
-}
regConfigToValueExpression :: RegConfig -> ValueExpression
regConfigToValueExpression (RegConfig regConfig) =
  RawSql.unsafeFromRawSql
    ( RawSql.fromString "\'"
        <> regConfig
        <> RawSql.fromString "\'"
    )

{- | Converts a 'ValueExpression' to a 'TSVector', optionally using a specified 'RegConfig'.

The provided 'ValueExpression' must adhere to the following limitations:
  * It must evaluate to one of the following types: @text@, @json@, or @jsonb@.
  * Behavior may differ depending on whether @json@ or @jsonb@ is used.
  * Word normalization will be applied during the conversion process.

@since 1.1.0.0
-}
toTSVector :: ValueExpression -> Maybe RegConfig -> TSVector
toTSVector val mbRegConfig =
  TSVector
    . RawSql.toRawSql
    . functionCall toTSVectorFunction
    $ case mbRegConfig of
      Nothing -> [val]
      Just regConfig -> [regConfigToValueExpression regConfig, val]

{- | Converts a 'ValueExpression' to a 'TSQuery', optionally using a specified 'RegConfig'.

The provided 'ValueExpression' must adhere to the following limitations:
  * It must evaluate to one of the following types: @text@, @json@, or @jsonb@.
  * Behavior may differ depending on whether @json@ or @jsonb@ is used.
  * Word normalization will be applied during the conversion process.

@since 1.1.0.0
-}
toTSQuery :: ValueExpression -> Maybe RegConfig -> TSQuery
toTSQuery val mbRegConfig =
  TSQuery
    . RawSql.toRawSql
    . functionCall toTSQueryFunction
    $ case mbRegConfig of
      Nothing -> [val]
      Just regConfig -> [regConfigToValueExpression regConfig, val]

{- | Converts a 'TSQuery' to a 'ValueExpression'.

@since 1.1.0.0
-}
tsQueryToValueExpression :: TSQuery -> ValueExpression
tsQueryToValueExpression (TSQuery rawSql) = RawSql.unsafeFromRawSql rawSql

{- | Converts a 'TSVector' to a 'ValueExpression'.

@since 1.1.0.0
-}
tsVectorToValueExpression :: TSVector -> ValueExpression
tsVectorToValueExpression (TSVector rawSql) = RawSql.unsafeFromRawSql rawSql

{- | Perform a text search ranking based on how well the 'TSVector' and 'TSQuery' match.

@since 1.1.0.0
-}
toTSRank :: TSVector -> TSQuery -> ValueExpression
toTSRank tsVector tsQuery =
  functionCall
    toTSRankFunction
    [ tsVectorToValueExpression tsVector
    , tsQueryToValueExpression tsQuery
    ]

{- | Assigns the given weight to each element of a 'TSVector'.

@since 1.1.0.0
-}
setTSWeight :: TSVector -> TSWeight -> TSVector
setTSWeight tsVector tsWeight =
  TSVector . RawSql.toRawSql $
    functionCall
      setTSWeightFunction
      [tsVectorToValueExpression tsVector, tsWeightToValueExpression tsWeight]

{- | Converts a 'TSWeight' to a 'ValueExpression'.

@since 1.1.0.0
-}
tsWeightToValueExpression :: TSWeight -> ValueExpression
tsWeightToValueExpression (TSWeight rawSql) = RawSql.unsafeFromRawSql rawSql

{- | Converts a 'ValueExpression' into a 'TSQuery', optionally using a specified 'RegConfig'.  The
     'ValueExpression' must be text. All punctuation in the given 'ValueExpression' will be ignored
     and individual words will be combined with a logical AND. This results in a 'TSQuery' that
     matches when all words are present.

@since 1.1.0.0
-}
plainToTSQuery :: ValueExpression -> Maybe RegConfig -> TSQuery
plainToTSQuery val mbRegConfig =
  TSQuery
    . RawSql.toRawSql
    . functionCall plaintoTSQueryFunction
    $ case mbRegConfig of
      Nothing -> [val]
      Just regConfig -> [regConfigToValueExpression regConfig, val]

{- | Type to represent a text search configuration.

'RegConfig' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

 @since 1.1.0.0
-}
newtype RegConfig
  = RegConfig RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The "arabic" text search configuration.

@since 1.1.0.0
-}
arabicRegConfig :: RegConfig
arabicRegConfig = RegConfig $ RawSql.fromString "arabic"

{- | The "danish" text search configuration.

@since 1.1.0.0
-}
danishRegConfig :: RegConfig
danishRegConfig = RegConfig $ RawSql.fromString "danish"

{- | The "dutch" text search configuration.

@since 1.1.0.0
-}
dutchRegConfig :: RegConfig
dutchRegConfig = RegConfig $ RawSql.fromString "dutch"

{- | The "english" text search configuration.

@since 1.1.0.0
-}
englishRegConfig :: RegConfig
englishRegConfig = RegConfig $ RawSql.fromString "english"

{- | The "finnish" text search configuration.

@since 1.1.0.0
-}
finnishRegConfig :: RegConfig
finnishRegConfig = RegConfig $ RawSql.fromString "finnish"

{- | The "french" text search configuration.

@since 1.1.0.0
-}
frenchRegConfig :: RegConfig
frenchRegConfig = RegConfig $ RawSql.fromString "french"

{- | The "german" text search configuration.

@since 1.1.0.0
-}
germanRegConfig :: RegConfig
germanRegConfig = RegConfig $ RawSql.fromString "german"

{- | The "greek" text search configuration.

@since 1.1.0.0
-}
greekRegConfig :: RegConfig
greekRegConfig = RegConfig $ RawSql.fromString "greek"

{- | The "hungarian" text search configuration.

@since 1.1.0.0
-}
hungarianRegConfig :: RegConfig
hungarianRegConfig = RegConfig $ RawSql.fromString "hungarian"

{- | The "indonesian" text search configuration.

@since 1.1.0.0
-}
indonesianRegConfig :: RegConfig
indonesianRegConfig = RegConfig $ RawSql.fromString "indonesian"

{- | The "irish" text search configuration.

@since 1.1.0.0
-}
irishRegConfig :: RegConfig
irishRegConfig = RegConfig $ RawSql.fromString "irish"

{- | The "italian" text search configuration.

@since 1.1.0.0
-}
italianRegConfig :: RegConfig
italianRegConfig = RegConfig $ RawSql.fromString "italian"

{- | The "lithuanian" text search configuration.

@since 1.1.0.0
-}
lithuanianRegConfig :: RegConfig
lithuanianRegConfig = RegConfig $ RawSql.fromString "lithuanian"

{- | The "nepali" text search configuration.

@since 1.1.0.0
-}
nepaliRegConfig :: RegConfig
nepaliRegConfig = RegConfig $ RawSql.fromString "nepali"

{- | The "norwegian" text search configuration.

@since 1.1.0.0
-}
norwegianRegConfig :: RegConfig
norwegianRegConfig = RegConfig $ RawSql.fromString "norwegian"

{- | The "portuguese" text search configuration.

@since 1.1.0.0
-}
portugueseRegConfig :: RegConfig
portugueseRegConfig = RegConfig $ RawSql.fromString "portuguese"

{- | The "romanian" text search configuration.

@since 1.1.0.0
-}
romanianRegConfig :: RegConfig
romanianRegConfig = RegConfig $ RawSql.fromString "romanian"

{- | The "russian" text search configuration.

@since 1.1.0.0
-}
russianRegConfig :: RegConfig
russianRegConfig = RegConfig $ RawSql.fromString "russian"

{- | The "simple" text search configuration.

@since 1.1.0.0
-}
simpleRegConfig :: RegConfig
simpleRegConfig = RegConfig $ RawSql.fromString "simple"

{- | The "spanish" text search configuration.

@since 1.1.0.0
-}
spanishRegConfig :: RegConfig
spanishRegConfig = RegConfig $ RawSql.fromString "spanish"

{- | The "swedish" text search configuration.

@since 1.1.0.0
-}
swedishRegConfig :: RegConfig
swedishRegConfig = RegConfig $ RawSql.fromString "swedish"

{- | The "tamil" text search configuration.

@since 1.1.0.0
-}
tamilRegConfig :: RegConfig
tamilRegConfig = RegConfig $ RawSql.fromString "tamil"

{- | The "turkish" text search configuration.

@since 1.1.0.0
-}
turkishRegConfig :: RegConfig
turkishRegConfig = RegConfig $ RawSql.fromString "turkish"
