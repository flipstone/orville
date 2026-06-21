module Test.PgTime
  ( pgTimeTests
  )
where

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.String as String
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Raw.PgTime as PgTime

import qualified Test.Property as Property

pgTimeTests :: Orville.ConnectionPool -> Tasty.TestTree
pgTimeTests _pool =
  Tasty.testGroup
    "PgTime"
    [ TastyHH.testProperty "Handles seconds in UTC offset correctly" prop_secondsInUTCOffset
    , TastyHH.testProperty "Handles far future" prop_farFuture
    ]

prop_secondsInUTCOffset :: HH.Property
prop_secondsInUTCOffset =
  Property.singletonProperty $
    parseOnly PgTime.utcTime (String.fromString "1971-01-01 00:00:00-00:44:30")
      === Right (UTCTime (fromGregorian 1971 1 1) (44 * 60 + 30))

prop_farFuture :: HH.Property
prop_farFuture =
  Property.singletonProperty $
    -- https://github.com/postgres/postgres/blob/b5737efea00717173c0cc889ebd115966abd8c8c/src/test/regress/sql/timestamptz.sql#L175
    parseOnly PgTime.utcTime (String.fromString "294276-12-31 23:59:59+00")
      === Right (UTCTime (fromGregorian 294276 12 31) (23 * 3600 + 59 * 60 + 59))
