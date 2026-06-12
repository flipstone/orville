module Test.Expr.Cursor
  ( cursorTests
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NE
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar, assertEqualFooBarRows, findAllFooBars, mkFooBar, withFooBarData)
import qualified Test.Property as Property

cursorTests :: Orville.ConnectionPool -> Tasty.TestTree
cursorTests pool =
  Tasty.testGroup
    "Expr - Cursor"
    [ TastyHH.testProperty "In transaction" (prop_cursorInTransaction pool)
    , TastyHH.testProperty "Outside transaction (with hold)" (prop_cursorOutsideTransactionWithHold pool)
    , TastyHH.testProperty "Close all cursors" (prop_cursorCloseAll pool)
    , TastyHH.testProperty "Move" (prop_cursorMove pool)
    , TastyHH.testProperty "No scroll" (prop_cursorNoScroll pool)
    , TastyHH.testProperty "Fetch all" (prop_cursorFetchAll pool)
    , TastyHH.testProperty "Fetch row count" (prop_cursorFetchRowCount pool)
    , TastyHH.testProperty "Fetch forward" (prop_cursorFetchForward pool)
    , TastyHH.testProperty "Fetch forward count" (prop_cursorFetchForwardCount pool)
    , TastyHH.testProperty "Fetch forward all" (prop_cursorFetchForwardAll pool)
    , TastyHH.testProperty "Fetch backward" (prop_cursorFetchBackward pool)
    , TastyHH.testProperty "Fetch backward count" (prop_cursorFetchBackwardCount pool)
    , TastyHH.testProperty "Fetch backward all" (prop_cursorFetchBackwardAll pool)
    , TastyHH.testProperty "Fetch first/last" (prop_cursorFetchFirstLast pool)
    , TastyHH.testProperty "Fetch next/prior" (prop_cursorFetchNextPrior pool)
    , TastyHH.testProperty "Fetch absolute" (prop_cursorFetchAbsolute pool)
    , TastyHH.testProperty "Fetch relative" (prop_cursorFetchRelative pool)
    ]

prop_cursorInTransaction :: Orville.ConnectionPool -> HH.Property
prop_cursorInTransaction pool =
  Property.singletonProperty $ do
    result <-
      withFooBarData pool (NE.fromList [row 1, row 2]) $ \connection ->
        withTestTransaction connection $
          withTestCursor connection Nothing Nothing findAllFooBars $ \cursorName -> do
            result <- RawSql.execute connection $ Expr.fetch Nothing cursorName
            Execution.readRows result

    assertEqualFooBarRows result [row 1]

prop_cursorOutsideTransactionWithHold :: Orville.ConnectionPool -> HH.Property
prop_cursorOutsideTransactionWithHold pool =
  Property.singletonProperty $ do
    result <-
      withFooBarData pool (NE.fromList [row 1, row 2]) $ \connection ->
        withTestCursor connection Nothing (Just Expr.withHold) findAllFooBars $ \cursorName -> do
          result <- RawSql.execute connection $ Expr.fetch Nothing cursorName
          Execution.readRows result

    assertEqualFooBarRows result [row 1]

prop_cursorCloseAll :: Orville.ConnectionPool -> HH.Property
prop_cursorCloseAll pool =
  Property.singletonProperty $ do
    MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
      let
        cursorName :: Expr.CursorName
        cursorName = Expr.fromIdentifier $ Expr.identifier "testcursor"

        declare =
          RawSql.executeVoid connection
            . Expr.declare cursorName Nothing (Just Expr.withHold)
            $ RawSql.unsafeSqlExpression "SELECT 1"

        close =
          RawSql.executeVoid connection $ Expr.close (Left Expr.allCursors)

      -- As long as close doesn't raise an exception, the test passes
      ExSafe.bracket_ declare close (pure ())

prop_cursorMove :: Orville.ConnectionPool -> HH.Property
prop_cursorMove pool =
  Property.singletonProperty $ do
    result <-
      withFooBarData pool (NE.fromList [row 1, row 2, row 3]) $ \connection ->
        withTestCursor connection Nothing (Just Expr.withHold) findAllFooBars $ \cursorName -> do
          RawSql.executeVoid connection $ Expr.move (Just $ Expr.rowCount 2) cursorName
          result <- RawSql.execute connection $ Expr.fetch Nothing cursorName
          Execution.readRows result

    assertEqualFooBarRows result [row 3]

prop_cursorNoScroll :: Orville.ConnectionPool -> HH.Property
prop_cursorNoScroll pool =
  Property.singletonProperty $ do
    scrollBackResult <-
      withFooBarData pool (NE.fromList [row 1, row 2]) $ \connection ->
        withTestCursor connection (Just Expr.noScroll) (Just Expr.withHold) findAllFooBars $ \cursorName -> do
          RawSql.executeVoid connection $ Expr.move (Just Expr.next) cursorName
          ExSafe.try $ RawSql.executeVoid connection $ Expr.move (Just Expr.prior) cursorName

    case scrollBackResult of
      Right () -> do
        HH.footnote "Expected 'executeVoid' to return failure, but it did not"
        HH.failure
      Left err ->
        -- Expected that the execute failed because we tried to scroll backward
        -- on a non-scrollable cursor
        Conn.sqlExecutionErrorSqlState err === Just (B8.pack "55000")

prop_cursorFetchAll :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchAll pool =
  Property.singletonProperty $ do
    [first] <-
      runFetchDirectionsOnData
        pool
        Nothing
        (NE.fromList [row 1, row 2])
        [Expr.fetchAll]

    assertEqualFooBarRows first [row 1, row 2]

prop_cursorFetchRowCount :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchRowCount pool =
  Property.singletonProperty $ do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        Nothing
        (NE.fromList [row 1, row 2, row 3])
        [Expr.rowCount 2, Expr.rowCount 2]

    assertEqualFooBarRows first [row 1, row 2]
    assertEqualFooBarRows second [row 3]

prop_cursorFetchForward :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchForward pool =
  Property.singletonProperty $ do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        Nothing
        (NE.fromList [row 1, row 2])
        [Expr.forward, Expr.forward]

    assertEqualFooBarRows first [row 1]
    assertEqualFooBarRows second [row 2]

prop_cursorFetchForwardCount :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchForwardCount pool =
  Property.singletonProperty $ do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        Nothing
        (NE.fromList [row 1, row 2, row 3])
        [Expr.forwardCount 2, Expr.forwardCount 2]

    assertEqualFooBarRows first [row 1, row 2]
    assertEqualFooBarRows second [row 3]

prop_cursorFetchForwardAll :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchForwardAll pool =
  Property.singletonProperty $ do
    [first] <-
      runFetchDirectionsOnData
        pool
        Nothing
        (NE.fromList [row 1, row 2])
        [Expr.forwardAll]

    assertEqualFooBarRows first [row 1, row 2]

prop_cursorFetchBackward :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchBackward pool =
  Property.singletonProperty $ do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        (NE.fromList [row 1, row 2])
        [Expr.forwardCount 2, Expr.backward]

    assertEqualFooBarRows first [row 1, row 2]
    assertEqualFooBarRows second [row 1]

prop_cursorFetchBackwardCount :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchBackwardCount pool =
  Property.singletonProperty $ do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        (NE.fromList [row 1, row 2, row 3])
        [Expr.forwardCount 3, Expr.backwardCount 2]

    assertEqualFooBarRows first [row 1, row 2, row 3]
    assertEqualFooBarRows second [row 2, row 1]

prop_cursorFetchBackwardAll :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchBackwardAll pool =
  Property.singletonProperty $ do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        (NE.fromList [row 1, row 2, row 3])
        [Expr.forwardCount 4, Expr.backwardAll]

    assertEqualFooBarRows first [row 1, row 2, row 3]
    assertEqualFooBarRows second [row 3, row 2, row 1]

prop_cursorFetchFirstLast :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchFirstLast pool =
  Property.singletonProperty $ do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        Nothing
        (NE.fromList [row 1, row 2, row 3])
        [Expr.first, Expr.last]

    assertEqualFooBarRows first [row 1]
    assertEqualFooBarRows second [row 3]

prop_cursorFetchNextPrior :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchNextPrior pool =
  Property.singletonProperty $ do
    [first, second, third] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        (NE.fromList [row 1, row 2, row 3])
        [Expr.next, Expr.next, Expr.prior]

    assertEqualFooBarRows first [row 1]
    assertEqualFooBarRows second [row 2]
    assertEqualFooBarRows third [row 1]

prop_cursorFetchAbsolute :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchAbsolute pool =
  Property.singletonProperty $ do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        (NE.fromList [row 1, row 2, row 3])
        [Expr.absolute 3, Expr.absolute (-1)]

    assertEqualFooBarRows first [row 3]
    assertEqualFooBarRows second [row 3]

prop_cursorFetchRelative :: Orville.ConnectionPool -> HH.Property
prop_cursorFetchRelative pool =
  Property.singletonProperty $ do
    [first, second, third, fourth] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        (NE.fromList [row 1, row 2, row 3])
        [Expr.relative 1, Expr.relative 2, Expr.relative (-1), Expr.relative 0]

    assertEqualFooBarRows first [row 1]
    assertEqualFooBarRows second [row 3]
    assertEqualFooBarRows third [row 2]
    assertEqualFooBarRows fourth [row 2]

row :: Int.Int32 -> FooBar
row n = mkFooBar n ("row " <> show n)

runFetchDirectionsOnData ::
  Orville.ConnectionPool ->
  Maybe Expr.ScrollExpr ->
  NE.NonEmpty FooBar ->
  [Expr.CursorDirection] ->
  HH.PropertyT IO [[[(Maybe B8.ByteString, SqlValue.SqlValue)]]]
runFetchDirectionsOnData pool scroll fooBars directions =
  withFooBarData pool fooBars $ \connection ->
    withTestCursor connection scroll (Just Expr.withHold) findAllFooBars $ \cursorName ->
      let
        runDirection direction = do
          result <- RawSql.execute connection $ Expr.fetch (Just direction) cursorName
          Execution.readRows result
      in
        traverse runDirection directions

withTestCursor ::
  Orville.Connection ->
  Maybe Expr.ScrollExpr ->
  Maybe Expr.HoldExpr ->
  Expr.QueryExpr ->
  (Expr.CursorName -> IO a) ->
  IO a
withTestCursor connection scroll hold query action =
  let
    cursorName :: Expr.CursorName
    cursorName = Expr.fromIdentifier $ Expr.identifier "testcursor"

    declare =
      RawSql.executeVoid connection $
        Expr.declare cursorName scroll hold query

    close =
      RawSql.executeVoid connection $ Expr.close (Right cursorName)
  in
    ExSafe.bracket_ declare close (action cursorName)

withTestTransaction :: Orville.Connection -> IO a -> IO a
withTestTransaction connection action =
  let
    begin =
      RawSql.executeVoid connection $ Expr.beginTransaction Nothing

    commit =
      RawSql.executeVoid connection $ Expr.commit
  in
    ExSafe.bracket_ begin commit action
