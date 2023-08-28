{-# LANGUAGE CPP #-}
module Database.Orville.PostgreSQL.Plan.Explanation
  ( Explanation
  , noExplanation
  , explainStep
  , explanationSteps
  ) where

newtype Explanation =
  Explanation ([String] -> [String])

#if MIN_VERSION_base(4,11,0)
instance Semigroup Explanation where
  (<>) = appendExplanation
#endif

instance Monoid Explanation where
  mempty = noExplanation
  mappend = appendExplanation

appendExplanation :: Explanation -> Explanation -> Explanation
appendExplanation (Explanation front) (Explanation back) =
  Explanation (front . back)

noExplanation :: Explanation
noExplanation =
  Explanation id

explainStep :: String -> Explanation
explainStep str =
  Explanation (str:)

explanationSteps :: Explanation -> [String]
explanationSteps (Explanation prependTo) =
  prependTo []
