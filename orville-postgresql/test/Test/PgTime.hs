module Test.PgTime
  ( pgTimeTests
  )
where

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.String as String
import Data.Time (UTCTime (..), fromGregorian)
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Raw.PgTime as PgTime

import qualified Test.Property as Property

pgTimeTests :: Orville.Pool Orville.Connection -> Property.Group
pgTimeTests _pool =
  Property.group "PgTime" $
    [
      ( String.fromString "Handles seconds in UTC offset correctly"
      , Property.singletonProperty $
          parseOnly PgTime.utcTime (String.fromString "1971-01-01 00:00:00-00:44:30")
            HH.=== Right (UTCTime (fromGregorian 1971 1 1) (44 * 60 + 30))
      )
    ,
      ( String.fromString "Handles far future"
      , Property.singletonProperty $
          -- https://github.com/postgres/postgres/blob/b5737efea00717173c0cc889ebd115966abd8c8c/src/test/regress/sql/timestamptz.sql#L175
          parseOnly PgTime.utcTime (String.fromString "294276-12-31 23:59:59+00")
            HH.=== Right (UTCTime (fromGregorian 294276 12 31) (23 * 3600 + 59 * 60 + 59))
      )
    ]
