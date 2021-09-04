module Test.Property
  ( singletonProperty,
    Group (..),
    group,
    checkGroups,
    checkGroup,
    allPassed,
  )
where

import qualified Control.Monad as Monad
import qualified GHC.Stack as CallStack
import qualified Hedgehog as HH
import qualified Hedgehog.Internal.Config as Config
import qualified Hedgehog.Internal.Property as Property
import qualified Hedgehog.Internal.Report as Report
import qualified Hedgehog.Internal.Runner as Runner
import qualified Hedgehog.Internal.Seed as Seed

singletonProperty :: CallStack.HasCallStack => HH.PropertyT IO () -> HH.Property
singletonProperty = HH.withTests 1 . HH.property

data Group = Group
  { groupName :: String
  , groupProperties :: [(HH.PropertyName, HH.Property)]
  }

group :: String -> [(HH.PropertyName, HH.Property)] -> Group
group = Group

checkGroups :: Foldable f => f Group -> IO Report.Summary
checkGroups groups = do
  useColor <- Config.resolveColor Nothing
  summary <- foldMap (checkGroup useColor) groups
  putStrLn =<< Report.renderSummary useColor summary
  pure summary

checkGroup :: Config.UseColor -> Group -> IO Report.Summary
checkGroup useColor propGroup = do
  let name = groupName propGroup
      properties = groupProperties propGroup

  putStrLn $ "• " <> name <> " : " <> show (length properties) <> " properties"
  foldMap (checkProperty useColor) properties

checkProperty :: Config.UseColor -> (HH.PropertyName, HH.Property) -> IO Report.Summary
checkProperty useColor (name, prop) = do
  seed <- Seed.random
  report <-
    Runner.checkReport
      (Property.propertyConfig prop)
      0
      seed
      (Property.propertyTest prop)
      (\_ -> pure ())

  let result = Report.reportStatus report

  Monad.when (result /= Report.OK) $ do
    putStrLn =<< Report.renderResult useColor (Just name) report

  pure $ Report.fromResult result

allPassed :: Report.Summary -> Bool
allPassed summary =
  Report.summaryFailed summary == 0
    && Report.summaryGaveUp summary == 0
