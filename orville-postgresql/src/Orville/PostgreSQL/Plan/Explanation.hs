{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Plan.Explanation
  ( Explanation
  , noExplanation
  , explainStep
  , explanationSteps
  )
where

{- | An 'Explanation' represents an example sequence of queries showing the steps
 would be executed by an Orville 'Orville.PostgreSQL.Plan.Operation.Operation'.

@since 1.0.0.0
-}
newtype Explanation
  = Explanation ([String] -> [String])

-- | @since 1.0.0.0
instance Semigroup Explanation where
  (<>) = appendExplanation

-- | @since 1.0.0.0
instance Monoid Explanation where
  mempty = noExplanation

{- | Appends two 'Explanation's with the steps from the first argument being shown
first.

@since 1.0.0.0
-}
appendExplanation :: Explanation -> Explanation -> Explanation
appendExplanation (Explanation front) (Explanation back) =
  Explanation (front . back)

{- | Constructs an empty 'Explanation'.

@since 1.0.0.0
-}
noExplanation :: Explanation
noExplanation =
  Explanation id

{- | Constructs an 'Explanation' with a single step.

@since 1.0.0.0
-}
explainStep :: String -> Explanation
explainStep str =
  Explanation (str :)

{- | Retrieves the steps contained in the 'Explanation'.

@since 1.0.0.0
-}
explanationSteps :: Explanation -> [String]
explanationSteps (Explanation prependTo) =
  prependTo []
