module Test.SqlType
  ( sqlTypeSpecs
  ) where

import Data.Pool (Pool)
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Time as Time
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, expectationFailure)

import Database.Orville.PostgreSQL.Connection (Connection, executeRawVoid, executeRaw)
import Database.Orville.PostgreSQL.Internal.ExecutionResult (decodeRows)
import Database.Orville.PostgreSQL.Internal.SqlType (-- numeric types
                                                      integer
                                                    , serial
                                                    , bigInteger
                                                    , bigserial
                                                    , double

                                                    -- textual-ish types
                                                    , boolean
                                                    , unboundedText
                                                    , fixedText
                                                    , boundedText
                                                    , textSearchVector

                                                    -- date types
                                                    , date
                                                    , timestamp
                                                    )
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr

sqlTypeSpecs :: Pool Connection -> Spec
sqlTypeSpecs pool = describe "Tests of SqlType decode" $ do
  integerSpecs pool
  bigIntegerSpecs pool
  serialSpecs pool
  bigSerialSpecs pool
  doubleSpecs pool
  boolSpecs pool
  unboundedTextSpecs pool
  fixedTextSpecs pool
  boundedTextSpecs pool
  textSearchVectorSpecs pool
  dateSpecs pool
  timestampSpecs pool

integerSpecs :: Pool Connection -> Spec
integerSpecs pool = do
  it "Testing the decode of INTEGER with value 0" $ do
    dropAndRecreateTable pool "myinteger" "INTEGER"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "myinteger") [B8.pack $ show (0 :: Int)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "myinteger"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res integer
        shouldBe maybeA (Just 0)

  it "Testing the decode of INTEGER with value 2147483647" $ do
    dropAndRecreateTable pool "myinteger" "INTEGER"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "myinteger") [B8.pack $ show (2147483647 :: Int)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "myinteger"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res integer
        shouldBe maybeA (Just 2147483647)

  it "Testing the decode of INTEGER with value -2147483648" $ do
    dropAndRecreateTable pool "myinteger" "INTEGER"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "myinteger") [B8.pack $ show (-2147483648 :: Int)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "myinteger"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res integer
        shouldBe maybeA (Just (-2147483648))

bigIntegerSpecs :: Pool Connection -> Spec
bigIntegerSpecs pool = do
  it "Testing the decode of BIGINT with value 0" $ do
    dropAndRecreateTable pool "mybiginteger" "BIGINT"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "mybiginteger") [B8.pack $ show (0 :: Int64)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "mybiginteger"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res bigInteger
        shouldBe maybeA (Just 0 :: Maybe Int64)

  it "Testing the decode of BIGINT with value 21474836470" $ do
    dropAndRecreateTable pool "mybiginteger" "BIGINT"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "mybiginteger") [B8.pack $ show (21474836470 :: Int64)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "mybiginteger"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res bigInteger
        shouldBe maybeA (Just 21474836470 :: Maybe Int64)

serialSpecs :: Pool Connection -> Spec
serialSpecs pool = do
  it "Testing the decode of SERIAL with value 0" $ do
    dropAndRecreateTable pool "myserial" "SERIAL"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "myserial") [B8.pack $ show (0 :: Int)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "myserial"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res serial
        shouldBe maybeA (Just 0)

  it "Testing the decode of SERIAL with value 2147483647" $ do
    dropAndRecreateTable pool "myserial" "SERIAL"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "myserial") [B8.pack $ show (2147483647 ::Int)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "myserial"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res serial
        shouldBe maybeA (Just 2147483647)

  it "Testing the decode of SERIAL with value -2147483648" $ do
    dropAndRecreateTable pool "myserial" "SERIAL"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "myserial") [B8.pack $ show (-2147483648 :: Int)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "myserial"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res serial
        shouldBe maybeA (Just (-2147483648))

bigSerialSpecs :: Pool Connection -> Spec
bigSerialSpecs pool = do
  it "Testing the decode of BIGSERIAL with value 0" $ do
    dropAndRecreateTable pool "mybigserial" "BIGSERIAL"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "mybigserial") [B8.pack $ show (0 :: Int64)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "mybigserial"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res bigserial
        shouldBe maybeA (Just 0 :: Maybe Int64)

  it "Testing the decode of BIGSERIAL with value 21474836470" $ do
    dropAndRecreateTable pool "mybigserial" "BIGSERIAL"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "mybigserial") [B8.pack $ show (21474836470 :: Int64)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "mybigserial"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res bigserial
        shouldBe maybeA (Just 21474836470 :: Maybe Int64)

doubleSpecs :: Pool Connection -> Spec
doubleSpecs pool = do
  it "Testing the decode of DOUBLE PRECISION with value 0" $ do
    dropAndRecreateTable pool "mydouble" "DOUBLE PRECISION"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "mydouble") [B8.pack $ show (0.0 :: Double)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "mydouble"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res double
        shouldBe maybeA (Just 0.0)

  it "Testing the decode of DOUBLE PRECISION with value 1.5" $ do
    dropAndRecreateTable pool "mydouble" "DOUBLE PRECISION"

    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr (B8.pack "mydouble") [B8.pack $ show (1.5 :: Double)])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr (B8.pack "mydouble"))
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res double
        shouldBe maybeA (Just 1.5)

boolSpecs :: Pool Connection -> Spec
boolSpecs pool = do
  it "Testing the decode of BOOL with value False" $ do
    dropAndRecreateTable pool "mybool" "BOOL"
    let tableBS = B8.pack "mybool"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack $ show False])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res boolean
        shouldBe maybeA (Just False)

  it "Testing the decode of BOOL with value True" $ do
    dropAndRecreateTable pool "mybool" "BOOL"
    let tableBS = B8.pack "mybool"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack $ show True])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res boolean
        shouldBe maybeA (Just True)

unboundedTextSpecs :: Pool Connection -> Spec
unboundedTextSpecs pool = do
  it "Testing the decode of TEXT with value abcde" $ do
    dropAndRecreateTable pool "myunboundedtext" "TEXT"
    let tableBS = B8.pack "myunboundedtext"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'abcde'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res unboundedText
        shouldBe maybeA (Just (T.pack "abcde"))

fixedTextSpecs :: Pool Connection -> Spec
fixedTextSpecs pool = do
  it "Testing the decode of CHAR(5) with value 'abcde'" $ do
    dropAndRecreateTable pool "myfixedtext" "CHAR(5)"
    let tableBS = B8.pack "myfixedtext"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'abcde'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res (fixedText 5)
        shouldBe maybeA (Just (T.pack "abcde"))

  it "Testing the decode of CHAR(5) with value 'fghi'" $ do
    dropAndRecreateTable pool "myfixedtext" "CHAR(5)"
    let tableBS = B8.pack "myfixedtext"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'fghi'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res (fixedText 5)
        shouldBe maybeA (Just (T.pack "fghi "))

boundedTextSpecs :: Pool Connection -> Spec
boundedTextSpecs pool = do
  it "Testing the decode of VARCHAR(5) with value 'abcde'" $ do
    dropAndRecreateTable pool "myboundedtext" "VARCHAR(5)"
    let tableBS = B8.pack "myboundedtext"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'abcde'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res (boundedText 5)
        shouldBe maybeA (Just (T.pack "abcde"))

  it "Testing the decode of VARCHAR(5) with value 'fghi'" $ do
    dropAndRecreateTable pool "myboundedtext" "VARCHAR(5)"
    let tableBS = B8.pack "myboundedtext"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'fghi'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res (boundedText 5)
        shouldBe maybeA (Just (T.pack "fghi"))

textSearchVectorSpecs :: Pool Connection -> Spec
textSearchVectorSpecs pool = do
  it "Testing the decode of TSVECTOR with value 'abcde'" $ do
    dropAndRecreateTable pool "mytsvector" "TSVECTOR"
    let tableBS = B8.pack "mytsvector"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'abcde'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res textSearchVector
        shouldBe maybeA (Just (T.pack "'abcde'"))

  it "Testing the decode of TSVECTOR with value 'fghi'" $ do
    dropAndRecreateTable pool "mytsvector" "TSVECTOR"
    let tableBS = B8.pack "mytsvector"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'fhgi'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res textSearchVector
        shouldBe maybeA (Just (T.pack "'fhgi'"))

dateSpecs :: Pool Connection -> Spec
dateSpecs pool = do
  it "Testing the decode of DATE with value 2020-12-21" $ do
    dropAndRecreateTable pool "mydate" "DATE"
    let tableBS = B8.pack "mydate"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'2020-12-21'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res date
        shouldBe maybeA (Just (Time.fromGregorian 2020 12 21))

timestampSpecs :: Pool Connection -> Spec
timestampSpecs pool = do
  it "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:00:32-00'" $ do
    dropAndRecreateTable pool "myutctime" "TIMESTAMP WITH TIME ZONE"
    let tableBS = B8.pack "myutctime"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'2020-12-21 00:00:32-00'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res timestamp
        shouldBe maybeA (Just (Time.UTCTime (Time.fromGregorian 2020 12 21) (Time.secondsToDiffTime 32)))

  it "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:00:32+00'" $ do
    dropAndRecreateTable pool "myutctime" "TIMESTAMP WITH TIME ZONE"
    let tableBS = B8.pack "myutctime"
    executeRawVoid pool $ Expr.insertExprToSql (Expr.InsertExpr tableBS [B8.pack "'2020-12-21 00:00:32+00'"])

    let selectBS = Expr.queryExprToSql (Expr.QueryExpr [B8.pack "*"] $ Expr.TableExpr tableBS)
    maybeResult <- executeRaw pool selectBS

    case maybeResult of
      Nothing ->
        expectationFailure "select returned nothing"

      Just res -> do
        (maybeA:_) <- decodeRows res timestamp
        shouldBe maybeA (Just (Time.UTCTime (Time.fromGregorian 2020 12 21) (Time.secondsToDiffTime 32)))

dropAndRecreateTable :: Pool Connection -> String -> String -> IO ()
dropAndRecreateTable pool tableName sqlTypeDDL' = do
  executeRawVoid pool . B8.pack $ "DROP TABLE IF EXISTS " <> tableName
  executeRawVoid pool . B8.pack $ "CREATE TABLE " <> tableName <> "(foo " <> sqlTypeDDL' <> ")"
