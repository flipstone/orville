module Test.Expr.Cursor
  ( cursorTests,
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar, assertEqualFooBarRows, dropAndRecreateTestTable, findAllFooBars, fooBarTable, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

cursorTests :: Pool.Pool Conn.Connection -> Property.Group
cursorTests pool =
  Property.group
    "Expr - Cursor"
    [ prop_cursorInTransaction pool
    , prop_cursorOutsideTransactionWithHold pool
    , prop_cursorCloseAll pool
    , prop_cursorMove pool
    , prop_cursorNoScroll pool
    , prop_cursorFetchAll pool
    , prop_cursorFetchCount pool
    , prop_cursorFetchForward pool
    , prop_cursorFetchForwardCount pool
    , prop_cursorFetchForwardAll pool
    , prop_cursorFetchBackward pool
    , prop_cursorFetchBackwardCount pool
    , prop_cursorFetchBackwardAll pool
    , prop_cursorFetchFirstLast pool
    , prop_cursorFetchNextPrior pool
    , prop_cursorFetchAbsolute pool
    , prop_cursorFetchRelative pool
    ]

prop_cursorInTransaction :: Property.NamedDBProperty
prop_cursorInTransaction =
  Property.singletonNamedDBProperty "In transaction" $ \pool -> do
    result <-
      withFooBarData pool [row 1, row 2] $ \connection ->
        withTestTransaction connection $
          withTestCursor connection Nothing Nothing findAllFooBars $ \cursorName -> do
            result <- RawSql.execute connection $ Expr.fetch Nothing cursorName
            ExecResult.readRows result

    assertEqualFooBarRows result [row 1]

prop_cursorOutsideTransactionWithHold :: Property.NamedDBProperty
prop_cursorOutsideTransactionWithHold =
  Property.singletonNamedDBProperty "Outside transaction (with hold)" $ \pool -> do
    result <-
      withFooBarData pool [row 1, row 2] $ \connection ->
        withTestCursor connection Nothing (Just Expr.withHold) findAllFooBars $ \cursorName -> do
          result <- RawSql.execute connection $ Expr.fetch Nothing cursorName
          ExecResult.readRows result

    assertEqualFooBarRows result [row 1]

prop_cursorCloseAll :: Property.NamedDBProperty
prop_cursorCloseAll =
  Property.singletonNamedDBProperty "Close all cursors" $ \pool -> do
    MIO.liftIO . Pool.withResource pool $ \connection -> do
      let cursorName :: Expr.CursorName
          cursorName = Expr.fromIdentifier $ Expr.identifier "testcursor"

          declare =
            RawSql.executeVoid connection $
              Expr.declare cursorName Nothing (Just Expr.withHold) $
                RawSql.unsafeFromRawSql $ RawSql.fromString "SELECT 1"

          close =
            RawSql.executeVoid connection $ Expr.close (Left Expr.allCursors)

      -- As long as close doesn't raise an exception, the test passes
      ExSafe.bracket_ declare close (pure ())

prop_cursorMove :: Property.NamedDBProperty
prop_cursorMove =
  Property.singletonNamedDBProperty "Move" $ \pool -> do
    result <-
      withFooBarData pool [row 1, row 2, row 3] $ \connection ->
        withTestCursor connection Nothing (Just Expr.withHold) findAllFooBars $ \cursorName -> do
          RawSql.executeVoid connection $ Expr.move (Just $ Expr.count 2) cursorName
          result <- RawSql.execute connection $ Expr.fetch Nothing cursorName
          ExecResult.readRows result

    assertEqualFooBarRows result [row 3]

prop_cursorNoScroll :: Property.NamedDBProperty
prop_cursorNoScroll =
  Property.singletonNamedDBProperty "Move" $ \pool -> do
    scrollBackResult <-
      withFooBarData pool [row 1, row 2] $ \connection ->
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

prop_cursorFetchAll :: Property.NamedDBProperty
prop_cursorFetchAll =
  Property.singletonNamedDBProperty "Fetch all" $ \pool -> do
    [first] <-
      runFetchDirectionsOnData
        pool
        Nothing
        [row 1, row 2]
        [Expr.fetchAll]

    assertEqualFooBarRows first [row 1, row 2]

prop_cursorFetchCount :: Property.NamedDBProperty
prop_cursorFetchCount =
  Property.singletonNamedDBProperty "Fetch count" $ \pool -> do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        Nothing
        [row 1, row 2, row 3]
        [Expr.count 2, Expr.count 2]

    assertEqualFooBarRows first [row 1, row 2]
    assertEqualFooBarRows second [row 3]

prop_cursorFetchForward :: Property.NamedDBProperty
prop_cursorFetchForward =
  Property.singletonNamedDBProperty "Fetch forward" $ \pool -> do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        Nothing
        [row 1, row 2]
        [Expr.forward, Expr.forward]

    assertEqualFooBarRows first [row 1]
    assertEqualFooBarRows second [row 2]

prop_cursorFetchForwardCount :: Property.NamedDBProperty
prop_cursorFetchForwardCount =
  Property.singletonNamedDBProperty "Fetch forward count" $ \pool -> do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        Nothing
        [row 1, row 2, row 3]
        [Expr.forwardCount 2, Expr.forwardCount 2]

    assertEqualFooBarRows first [row 1, row 2]
    assertEqualFooBarRows second [row 3]

prop_cursorFetchForwardAll :: Property.NamedDBProperty
prop_cursorFetchForwardAll =
  Property.singletonNamedDBProperty "Fetch forward all" $ \pool -> do
    [first] <-
      runFetchDirectionsOnData
        pool
        Nothing
        [row 1, row 2]
        [Expr.forwardAll]

    assertEqualFooBarRows first [row 1, row 2]

prop_cursorFetchBackward :: Property.NamedDBProperty
prop_cursorFetchBackward =
  Property.singletonNamedDBProperty "Fetch backward" $ \pool -> do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        [row 1, row 2]
        [Expr.forwardCount 2, Expr.backward]

    assertEqualFooBarRows first [row 1, row 2]
    assertEqualFooBarRows second [row 1]

prop_cursorFetchBackwardCount :: Property.NamedDBProperty
prop_cursorFetchBackwardCount =
  Property.singletonNamedDBProperty "Fetch backward count" $ \pool -> do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        [row 1, row 2, row 3]
        [Expr.forwardCount 3, Expr.backwardCount 2]

    assertEqualFooBarRows first [row 1, row 2, row 3]
    assertEqualFooBarRows second [row 2, row 1]

prop_cursorFetchBackwardAll :: Property.NamedDBProperty
prop_cursorFetchBackwardAll =
  Property.singletonNamedDBProperty "Fetch backward all" $ \pool -> do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        [row 1, row 2, row 3]
        [Expr.forwardCount 4, Expr.backwardAll]

    assertEqualFooBarRows first [row 1, row 2, row 3]
    assertEqualFooBarRows second [row 3, row 2, row 1]

prop_cursorFetchFirstLast :: Property.NamedDBProperty
prop_cursorFetchFirstLast =
  Property.singletonNamedDBProperty "Fetch first/last" $ \pool -> do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        Nothing
        [row 1, row 2, row 3]
        [Expr.first, Expr.last]

    assertEqualFooBarRows first [row 1]
    assertEqualFooBarRows second [row 3]

prop_cursorFetchNextPrior :: Property.NamedDBProperty
prop_cursorFetchNextPrior =
  Property.singletonNamedDBProperty "Fetch next/prior" $ \pool -> do
    [first, second, third] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        [row 1, row 2, row 3]
        [Expr.next, Expr.next, Expr.prior]

    assertEqualFooBarRows first [row 1]
    assertEqualFooBarRows second [row 2]
    assertEqualFooBarRows third [row 1]

prop_cursorFetchAbsolute :: Property.NamedDBProperty
prop_cursorFetchAbsolute =
  Property.singletonNamedDBProperty "Fetch absolute" $ \pool -> do
    [first, second] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        [row 1, row 2, row 3]
        [Expr.absolute 3, Expr.absolute (-1)]

    assertEqualFooBarRows first [row 3]
    assertEqualFooBarRows second [row 3]

prop_cursorFetchRelative :: Property.NamedDBProperty
prop_cursorFetchRelative =
  Property.singletonNamedDBProperty "Fetch relative" $ \pool -> do
    [first, second, third, fourth] <-
      runFetchDirectionsOnData
        pool
        (Just Expr.scroll)
        [row 1, row 2, row 3]
        [Expr.relative 1, Expr.relative 2, Expr.relative (-1), Expr.relative 0]

    assertEqualFooBarRows first [row 1]
    assertEqualFooBarRows second [row 3]
    assertEqualFooBarRows third [row 2]
    assertEqualFooBarRows fourth [row 2]

row :: Int.Int32 -> FooBar
row n = mkFooBar n ("row" <> show n)

runFetchDirectionsOnData ::
  Pool.Pool Conn.Connection ->
  Maybe Expr.ScrollExpr ->
  [FooBar] ->
  [Expr.CursorDirection] ->
  HH.PropertyT IO [[[(Maybe B8.ByteString, SqlValue.SqlValue)]]]
runFetchDirectionsOnData pool scroll fooBars directions =
  withFooBarData pool fooBars $ \connection ->
    withTestCursor connection scroll (Just Expr.withHold) findAllFooBars $ \cursorName ->
      let runDirection direction = do
            result <- RawSql.execute connection $ Expr.fetch (Just direction) cursorName
            ExecResult.readRows result
       in traverse runDirection directions

withFooBarData ::
  Pool.Pool Conn.Connection ->
  [FooBar] ->
  (Conn.Connection -> IO a) ->
  HH.PropertyT IO a
withFooBarData pool fooBars action =
  MIO.liftIO $
    Pool.withResource pool $ \connection -> do
      dropAndRecreateTestTable connection

      RawSql.executeVoid connection $
        Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing

      action connection

withTestCursor ::
  Conn.Connection ->
  Maybe Expr.ScrollExpr ->
  Maybe Expr.HoldExpr ->
  Expr.QueryExpr ->
  (Expr.CursorName -> IO a) ->
  IO a
withTestCursor connection scroll hold query action =
  let cursorName :: Expr.CursorName
      cursorName = Expr.fromIdentifier $ Expr.identifier "testcursor"

      declare =
        RawSql.executeVoid connection $
          Expr.declare cursorName scroll hold query

      close =
        RawSql.executeVoid connection $ Expr.close (Right cursorName)
   in ExSafe.bracket_ declare close (action cursorName)

withTestTransaction :: Conn.Connection -> IO a -> IO a
withTestTransaction connection action =
  let begin =
        RawSql.executeVoid connection $ Expr.beginTransaction Nothing

      commit =
        RawSql.executeVoid connection $ Expr.commit
   in ExSafe.bracket_ begin commit action
