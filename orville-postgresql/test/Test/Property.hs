module Test.Property (singletonProperty) where

import qualified Hedgehog as HH

singletonProperty :: HH.PropertyT IO () -> HH.Property
singletonProperty = HH.withTests 1 . HH.property
