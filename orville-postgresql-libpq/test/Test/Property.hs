module Test.Property
  ( singletonProperty,
  )
where

import qualified GHC.Stack as CallStack
import qualified Hedgehog as HH

singletonProperty :: CallStack.HasCallStack => HH.PropertyT IO () -> HH.Property
singletonProperty = HH.withTests 1 . HH.property
