module Test.PgTime
  ( pgTimeTests,
  )
where

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Pool as Pool
import qualified Data.String as String
import Data.Time (UTCTime (..), fromGregorian)
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.PgTime as PgTime

import qualified Test.Property as Property

pgTimeTests :: Pool.Pool Conn.Connection -> Property.Group
pgTimeTests _pool =
  Property.group "PgTime" $
    [
      ( String.fromString "Ignores seconds in UTC offset correctly"
      , Property.singletonProperty $
          parseOnly PgTime.utcTime (String.fromString "1971-01-01 00:00:00-00:44:30")
            HH.=== Right (UTCTime (fromGregorian 1971 1 1) (44 * 60))
      )
    ]
