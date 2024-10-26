{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Schema.SequenceDefinition
  ( SequenceDefinition
  , mkSequenceDefinition
  , setSequenceSchema
  , sequenceIdentifier
  , sequenceName
  , sequenceIncrement
  , setSequenceIncrement
  , sequenceMinValue
  , setSequenceMinValue
  , sequenceMaxValue
  , setSequenceMaxValue
  , sequenceStart
  , setSequenceStart
  , sequenceCache
  , setSequenceCache
  , sequenceCycle
  , setSequenceCycle
  , mkCreateSequenceExpr
  )
where

import Data.Int (Int64)

import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Schema.SequenceIdentifier (SequenceIdentifier, sequenceIdQualifiedName, setSequenceIdSchema, unqualifiedNameToSequenceId)

{- | Contains the definition of a SQL sequence for Orville to use when creating
  the sequence and fetching values from it. You can create a
  'SequenceDefinition' with default values via 'mkSequenceDefinition' and then
  use the various set functions that are provided if you need to set specific
  attributes on the sequence.

@since 1.0.0.0
-}
data SequenceDefinition = SequenceDefinition
  { i_sequenceIdentifier :: SequenceIdentifier
  , i_sequenceIncrement :: Int64
  , i_sequenceMinValue :: Maybe Int64
  , i_sequenceMaxValue :: Maybe Int64
  , i_sequenceStart :: Maybe Int64
  , i_sequenceCache :: Int64
  , i_sequenceCycle :: Bool
  }
  deriving
    ( -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Show
    )

{- | Constructs an ascending 'SequenceDefinition' with increment 1 and cache
  1 that does not cycle. The sequence will start at 1 and count to the
  largest 'Int64' value.

@since 1.0.0.0
-}
mkSequenceDefinition :: String -> SequenceDefinition
mkSequenceDefinition name =
  SequenceDefinition
    { i_sequenceIdentifier = unqualifiedNameToSequenceId name
    , i_sequenceIncrement = 1
    , i_sequenceMinValue = Nothing
    , i_sequenceMaxValue = Nothing
    , i_sequenceStart = Nothing
    , i_sequenceCache = 1
    , i_sequenceCycle = False
    }

{- | Sets the sequence's schema to the name in the given 'String', which will be
  treated as a SQL identifier. If a sequence has a schema name set, it will be
  included as a qualifier on the sequence name for all queries involving the
  sequence.

@since 1.0.0.0
-}
setSequenceSchema ::
  String ->
  SequenceDefinition ->
  SequenceDefinition
setSequenceSchema schemaName sequenceDef =
  sequenceDef
    { i_sequenceIdentifier = setSequenceIdSchema schemaName (i_sequenceIdentifier sequenceDef)
    }

{- | Retrieves the 'SequenceIdentifier' for this sequence, which is set by the
  name provided to 'mkSequenceDefinition' and any calls made to
  'setSequenceSchema' thereafter.

@since 1.0.0.0
-}
sequenceIdentifier :: SequenceDefinition -> SequenceIdentifier
sequenceIdentifier = i_sequenceIdentifier

{- | Retrieves the 'Expr.Qualified' 'Expr.SequenceName' for the sequence that
  should be used to build SQL expressions involving it.

@since 1.0.0.0
-}
sequenceName :: SequenceDefinition -> Expr.QualifiedOrUnqualified Expr.SequenceName
sequenceName =
  sequenceIdQualifiedName . i_sequenceIdentifier

{- | Retrieves the increment value for the sequence.

@since 1.0.0.0
-}
sequenceIncrement :: SequenceDefinition -> Int64
sequenceIncrement = i_sequenceIncrement

{- | Sets the increment value for the sequence. The increment cannot be set to
  @0@ (PostgreSQL will raise an error when trying to create or modify the
  sequence in this case).

  If the increment is negative, the sequence will be descending. When no
  explicit start is set, a descending sequence begins at the max value.

@since 1.0.0.0
-}
setSequenceIncrement :: Int64 -> SequenceDefinition -> SequenceDefinition
setSequenceIncrement n sequenceDef =
  sequenceDef {i_sequenceIncrement = n}

{- | Retrieves the min value of the sequence. If no explicit minimum has been set,
  this returns @1@ for ascending sequences and 'minBound' for 'Int64' for
  descending sequences.

@since 1.0.0.0
-}
sequenceMinValue :: SequenceDefinition -> Int64
sequenceMinValue sequenceDef =
  case i_sequenceMinValue sequenceDef of
    Just minValue -> minValue
    Nothing ->
      if sequenceIncrement sequenceDef >= 0
        then 1
        else minBound

{- | Sets the min value for the sequence.

@since 1.0.0.0
-}
setSequenceMinValue :: Int64 -> SequenceDefinition -> SequenceDefinition
setSequenceMinValue n sequenceDef =
  sequenceDef {i_sequenceMinValue = Just n}

{- | Retrieves the max value of the sequence. If no explicit maximum has been set,
  this returns 'maxBound' for 'Int64' for ascending sequences and @-1@ for
  descending sequences.

@since 1.0.0.0
-}
sequenceMaxValue :: SequenceDefinition -> Int64
sequenceMaxValue sequenceDef =
  case i_sequenceMaxValue sequenceDef of
    Just maxValue -> maxValue
    Nothing ->
      if sequenceIncrement sequenceDef >= 0
        then maxBound
        else -1

{- | Sets the max value for the sequence.

@since 1.0.0.0
-}
setSequenceMaxValue :: Int64 -> SequenceDefinition -> SequenceDefinition
setSequenceMaxValue n sequenceDef =
  sequenceDef {i_sequenceMaxValue = Just n}

{- | Retrieves the start value for the sequence. If no explicit start value has
  been set, this returns 'sequenceMinValue' for ascending sequences and
  'sequenceMaxValue' for descending sequences.

@since 1.0.0.0
-}
sequenceStart :: SequenceDefinition -> Int64
sequenceStart sequenceDef =
  case i_sequenceStart sequenceDef of
    Just start -> start
    Nothing ->
      if sequenceIncrement sequenceDef >= 0
        then sequenceMinValue sequenceDef
        else sequenceMaxValue sequenceDef

{- | Sets the sequence start value. The start value must be at least the
  minimum value and no greater than the maximum value.

@since 1.0.0.0
-}
setSequenceStart :: Int64 -> SequenceDefinition -> SequenceDefinition
setSequenceStart n sequenceDef =
  sequenceDef {i_sequenceStart = Just n}

{- | Retrieves the number of sequence values that will be pre-allocated by
  PostgreSQL.

@since 1.0.0.0
-}
sequenceCache :: SequenceDefinition -> Int64
sequenceCache = i_sequenceCache

{- | Sets the number of sequence values that will be pre-allocated by PostgreSQL.

@since 1.0.0.0
-}
setSequenceCache :: Int64 -> SequenceDefinition -> SequenceDefinition
setSequenceCache n sequenceDef =
  sequenceDef {i_sequenceCache = n}

{- | Indicates whether the sequence will wrap around when it reaches the maximum
  value (for ascending sequences) or minimum value (for descending sequences).
  When 'False', any attempts to get the next value of the sequence while at the
  limit will result in an error.

@since 1.0.0.0
-}
sequenceCycle :: SequenceDefinition -> Bool
sequenceCycle = i_sequenceCycle

{- | Sets the 'sequenceCycle' value for the sequence. 'True' indicates that the
  sequence will cycle. 'False' will cause an error to be raised if the next
  sequence value is requested while already at the limit.

@since 1.0.0.0
-}
setSequenceCycle :: Bool -> SequenceDefinition -> SequenceDefinition
setSequenceCycle b sequenceDef =
  sequenceDef {i_sequenceCycle = b}

{- | Builds a 'Expr.CreateSequenceExpr' that will create a SQL sequence matching
  the given 'SequenceDefinition' when it is executed.

@since 1.0.0.0
-}
mkCreateSequenceExpr :: SequenceDefinition -> Expr.CreateSequenceExpr
mkCreateSequenceExpr sequenceDef =
  Expr.createSequenceExpr
    (sequenceName sequenceDef)
    (Just . Expr.incrementBy . sequenceIncrement $ sequenceDef)
    (Just . Expr.minValue . sequenceMinValue $ sequenceDef)
    (Just . Expr.maxValue . sequenceMaxValue $ sequenceDef)
    (Just . Expr.startWith . sequenceStart $ sequenceDef)
    (Just . Expr.cache . sequenceCache $ sequenceDef)
    (Just . Expr.cycleIfTrue . sequenceCycle $ sequenceDef)
