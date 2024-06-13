{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Window.WindowDefinitionExpr
  ( WindowDefinitionExpr
  , windowDefinition
  , PartitionByExpr
  , partitionBy
  , FrameClause
  , frameClause
  , FrameModeExpr
  , rangeFrameMode
  , rowsFrameMode
  , groupsFrameMode
  , FrameStartExpr
  , unboundedPrecedingFrameStart
  , offsetPrecedingFrameStart
  , currentRowFrameStart
  , offsetFollowingFrameStart
  , FrameEndExpr
  , offsetPrecedingFrameEnd
  , currentRowFrameEnd
  , offsetFollowingFrameEnd
  , unboundedFollowingFrameEnd
  , FrameExclusionExpr
  , currentRowFrameExclusion
  , groupFrameExclusion
  , tiesFrameExclusion
  , noOthersFrameExclusion
  )
where

import qualified Data.List.NonEmpty as NEL
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Name (WindowName)
import qualified Orville.PostgreSQL.Expr.OrderBy as OrderBy
import qualified Orville.PostgreSQL.Expr.ValueExpression as ValueExpression
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL window definition. This can be used in either a @WINDOW@ clause or in the
portion of a window function call after the @OVER@.

'WindowDefinitionExpr' provides a 'RawSql.SqlExpression' instance.
See 'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype WindowDefinitionExpr = WindowDefinitionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Builds a 'WindowDefinitionExpr'. Note that it is up to the caller to ensure the options make
   sense togther, including particular rules below.

N.B. If the 'WindowName' is used (so we are "copying" a previous definition), then a few rules apply:
  * The 'ParititionByExpr' and 'OrderByClause' will be copied if any
  * If either 'ParitionByExpr' and 'OrderByClause' exist then a 'PartitionByExpr' may not be specified.
  * The 'OrderByClause' can _only_ be specified if the window copied does not have one.
  * The copied window must not specify a 'FrameClause'.

@since 1.1.0.0
-}
windowDefinition ::
  -- | An, optional, already existing named window that must be named in a prior @WINDOW@ entry.
  Maybe WindowName ->
  Maybe PartitionByExpr ->
  Maybe OrderBy.OrderByClause ->
  Maybe FrameClause ->
  WindowDefinitionExpr
windowDefinition mbWindowName mbPartitionBy mbOrderBy mbFrame =
  WindowDefinitionExpr
    . RawSql.intercalate RawSql.space
    $ catMaybes
      [ fmap RawSql.toRawSql mbWindowName
      , fmap RawSql.toRawSql mbPartitionBy
      , fmap RawSql.toRawSql mbOrderBy
      , fmap RawSql.toRawSql mbFrame
      ]

{- |
Type to represent the @PARTITION BY expression [, ...]@ portion of a SQL window definition.

'PartitionByExpr' provides a 'RawSql.SqlExpression' instance.
See 'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype PartitionByExpr = PartitionByExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
Builds a 'PartitionByExpr'.

@since 1.1.0.0
-}
partitionBy :: NEL.NonEmpty ValueExpression.ValueExpression -> PartitionByExpr
partitionBy exprs =
  PartitionByExpr $
    RawSql.fromString "PARTITION BY "
      <> RawSql.intercalate RawSql.commaSpace exprs

{- |
Type to represent the framing clause of a window definition.

'FrameClause' provides a 'RawSql.SqlExpression' instance.
See 'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype FrameClause = FrameClause RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Builds a 'FrameClause' from the given pieces.

@since 1.1.0.0
-}
frameClause :: Maybe FrameModeExpr -> FrameStartExpr -> Maybe FrameEndExpr -> Maybe FrameExclusionExpr -> FrameClause
frameClause mbFrameMode frameStart mbFrameEnd mbExclusion =
  let
    mode =
      maybe mempty RawSql.toRawSql mbFrameMode
    exclusion =
      maybe mempty RawSql.toRawSql mbExclusion
  in
    FrameClause $
      mode
        <> RawSql.space
        <> case mbFrameEnd of
          Nothing -> RawSql.toRawSql frameStart
          Just frameEnd ->
            RawSql.fromString "BETWEEN "
              <> RawSql.toRawSql frameStart
              <> RawSql.fromString " AND "
              <> RawSql.toRawSql frameEnd
        <> RawSql.space
        <> exclusion

{- |
Type to represent the mode of the framing in a window definition.

'FrameModeExpr' provides a 'RawSql.SqlExpression' instance.
See 'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype FrameModeExpr = FrameModeExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Value for the frame mode of @RANGE@

@since 1.1.0.0
-}
rangeFrameMode :: FrameModeExpr
rangeFrameMode = FrameModeExpr $ RawSql.fromString "RANGE"

{- | Value for the frame mode of @ROWS@

@since 1.1.0.0
-}
rowsFrameMode :: FrameModeExpr
rowsFrameMode = FrameModeExpr $ RawSql.fromString "ROWS"

{- | Value for the frame mode of @GROUPS@

@since 1.1.0.0
-}
groupsFrameMode :: FrameModeExpr
groupsFrameMode = FrameModeExpr $ RawSql.fromString "GROUPS"

{- |
Type to represent the beginning of the framing in a window definition.

'FrameStartExpr' provides a 'RawSql.SqlExpression' instance.
See 'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype FrameStartExpr = FrameStartExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Value for a frame start of @UNBOUNDED PRECEDING@

@since 1.1.0.0
-}
unboundedPrecedingFrameStart :: FrameStartExpr
unboundedPrecedingFrameStart = FrameStartExpr $ RawSql.fromString "UNBOUNDED PRECEDING"

{- | Value for a frame start of @expression PRECEDING@. Note that it is up to the caller to ensure
   that the 'ValueExpression' is valid in this context.

@since 1.1.0.0
-}
offsetPrecedingFrameStart :: ValueExpression.ValueExpression -> FrameStartExpr
offsetPrecedingFrameStart val = FrameStartExpr $ RawSql.toRawSql val <> RawSql.fromString " PRECEDING"

{- | Value for a frame start of @CURRENT ROW@

@since 1.1.0.0
-}
currentRowFrameStart :: FrameStartExpr
currentRowFrameStart = FrameStartExpr $ RawSql.fromString "CURRENT ROW"

{- | Value for a frame start of @expression FOLLOWING@. Note that it is up to the caller to ensure
   that the 'ValueExpression' is valid in this context.

@since 1.1.0.0
-}
offsetFollowingFrameStart :: ValueExpression.ValueExpression -> FrameStartExpr
offsetFollowingFrameStart val = FrameStartExpr $ RawSql.toRawSql val <> RawSql.fromString " FOLLOWING"

{- |
Type to represent the ending of the framing in a window definition.

'FrameEndExpr' provides a 'RawSql.SqlExpression' instance.
See 'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype FrameEndExpr = FrameEndExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Value for a frame end of @expression PRECEDING@. Note that it is up to the caller to ensure
   that the 'ValueExpression' is valid in this context.

@since 1.1.0.0
-}
offsetPrecedingFrameEnd :: ValueExpression.ValueExpression -> FrameEndExpr
offsetPrecedingFrameEnd val = FrameEndExpr $ RawSql.toRawSql val <> RawSql.fromString " PRECEDING"

{- | Value for a frame end of @CURRENT ROW@

@since 1.1.0.0
-}
currentRowFrameEnd :: FrameEndExpr
currentRowFrameEnd = FrameEndExpr $ RawSql.fromString "CURRENT ROW"

{- | Value for a frame end of @expression FOLLOWING@. Note that it is up to the caller to ensure
   that the 'ValueExpression' is valid in this context.

@since 1.1.0.0
-}
offsetFollowingFrameEnd :: ValueExpression.ValueExpression -> FrameEndExpr
offsetFollowingFrameEnd val = FrameEndExpr $ RawSql.toRawSql val <> RawSql.fromString " FOLLOWING"

{- | Value for a frame end of @UNBOUNDED FOLLOWING@

@since 1.1.0.0
-}
unboundedFollowingFrameEnd :: FrameEndExpr
unboundedFollowingFrameEnd = FrameEndExpr $ RawSql.fromString "UNBOUNDED FOLLOWING"

{- |
Type to represent the exclusion of results in the framing in a window definition.

'FrameExclusionExpr' provides a 'RawSql.SqlExpression' instance.
See 'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype FrameExclusionExpr = FrameExclusionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Value for a frame exclusion of @EXCLUDE CURRENT ROW@

@since 1.1.0.0
-}
currentRowFrameExclusion :: FrameExclusionExpr
currentRowFrameExclusion = FrameExclusionExpr $ RawSql.fromString "EXCLUDE CURRENT ROW"

{- | Value for a frame exclusion of @EXCLUDE GROUP@

@since 1.1.0.0
-}
groupFrameExclusion :: FrameExclusionExpr
groupFrameExclusion = FrameExclusionExpr $ RawSql.fromString "EXCLUDE GROUP"

{- | Value for a frame exclusion of @EXCLUDE TIES@

@since 1.1.0.0
-}
tiesFrameExclusion :: FrameExclusionExpr
tiesFrameExclusion = FrameExclusionExpr $ RawSql.fromString "EXCLUDE TIES"

{- | Value for a frame exclusion of @EXCLUDE NO OTHERS@

@since 1.1.0.0
-}
noOthersFrameExclusion :: FrameExclusionExpr
noOthersFrameExclusion = FrameExclusionExpr $ RawSql.fromString "EXCLUDE NO OTHERS"
