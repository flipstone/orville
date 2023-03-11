module Test.Expr.Time
  ( timeTests,
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.Int (Int32)
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Time as Time
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Text.Printf as Printf

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.Property as Property

timeTests :: Pool.Pool Conn.Connection -> Property.Group
timeTests pool =
  Property.group
    "Expr - Time"
    [ prop_now pool
    , prop_makeInterval pool
    ]

prop_now :: Property.NamedDBProperty
prop_now =
  Property.singletonNamedDBProperty "now()" $ \pool -> do
    let sql =
          Expr.queryExpr
            (Expr.selectClause (Expr.selectExpr Nothing))
            ( Expr.selectDerivedColumns
                [ Expr.deriveColumnAs Expr.now (Expr.columnName "result")
                ]
            )
            Nothing

        marshaller =
          Orville.annotateSqlMarshallerEmptyAnnotation $
            Orville.marshallField id (Orville.utcTimestampField "result")

    result <-
      HH.evalIO $
        Orville.runOrville pool $
          Orville.executeAndDecode Orville.SelectQuery sql marshaller

    today <- fmap Time.utctDay (HH.evalIO Time.getCurrentTime)
    fmap Time.utctDay result === [today]

prop_makeInterval :: Property.NamedDBProperty
prop_makeInterval =
  Property.namedDBProperty "make_interval()" $ \pool -> do
    intervalValues <- HH.forAllWith (T.unpack . renderExpectedValue) $ do
      years <- Gen.integral (Range.linear 1 100)
      months <- Gen.integral (Range.linear 1 11)
      weeks <- Gen.integral (Range.linear 1 3)
      days <- Gen.integral (Range.linear 1 6)
      hours <- Gen.integral (Range.linear 1 23)
      minutes <- Gen.integral (Range.linear 1 59)
      seconds <- Gen.integral (Range.linear 1 59)

      pure $
        [ (Expr.years, years)
        , (Expr.months, months)
        , (Expr.weeks, weeks)
        , (Expr.days, days)
        , (Expr.hours, hours)
        , (Expr.minutes, minutes)
        , (Expr.seconds, seconds)
        ]

    let intervalExpression =
          Expr.makeInterval $
            map
              (\(arg, value) -> (arg, Expr.valueExpression (SqlValue.fromInt32 value)))
              intervalValues

        sql =
          Expr.queryExpr
            (Expr.selectClause (Expr.selectExpr Nothing))
            ( Expr.selectDerivedColumns
                [ Expr.deriveColumnAs intervalExpression (Expr.columnName "result")
                ]
            )
            Nothing

        marshaller =
          Orville.annotateSqlMarshallerEmptyAnnotation $
            Orville.marshallField id (Orville.unboundedTextField "result")

    result <-
      HH.evalIO $
        Orville.runOrville pool $
          Orville.executeAndDecode Orville.SelectQuery sql marshaller

    result === [renderExpectedValue intervalValues]

renderExpectedValue :: [(Expr.IntervalArgument, Int32)] -> T.Text
renderExpectedValue values =
  let valuesByName =
        map
          (\(arg, value) -> (RawSql.toExampleBytes arg, value))
          values

      lookupInterval name =
        Maybe.fromMaybe 0 (lookup (B8.pack name) valuesByName)

      render :: Int32 -> String -> Maybe T.Text
      render value singularName =
        case value of
          0 -> Nothing
          1 -> Just (T.pack (Printf.printf "%d %s" value singularName))
          _ -> Just (T.pack (Printf.printf "%d %ss" value singularName))

      mbTime =
        case (lookupInterval "hours", lookupInterval "mins", lookupInterval "secs") of
          (0, 0, 0) -> Nothing
          (h, m, s) -> Just (T.pack (Printf.printf "%02d:%02d:%02d" h m s))
   in T.intercalate (T.pack " ") . Maybe.catMaybes $
        [ render (lookupInterval "years") "year"
        , render (lookupInterval "months") "mon"
        , render ((7 * lookupInterval "weeks") + lookupInterval "days") "day"
        , mbTime
        ]
