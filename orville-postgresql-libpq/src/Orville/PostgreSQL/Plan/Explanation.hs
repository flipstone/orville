module Orville.PostgreSQL.Plan.Explanation
  ( Explanation
  , noExplanation
  , explainStep
  , explanationSteps
  )
where

newtype Explanation
  = Explanation ([String] -> [String])

instance Semigroup Explanation where
  (<>) = appendExplanation

instance Monoid Explanation where
  mempty = noExplanation

appendExplanation :: Explanation -> Explanation -> Explanation
appendExplanation (Explanation front) (Explanation back) =
  Explanation (front . back)

noExplanation :: Explanation
noExplanation =
  Explanation id

explainStep :: String -> Explanation
explainStep str =
  Explanation (str :)

explanationSteps :: Explanation -> [String]
explanationSteps (Explanation prependTo) =
  prependTo []
