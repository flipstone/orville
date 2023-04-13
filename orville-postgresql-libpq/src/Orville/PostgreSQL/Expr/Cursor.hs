{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2022
License   : MIT
-}
module Orville.PostgreSQL.Expr.Cursor
  ( DeclareExpr,
    declare,
    ScrollExpr,
    scroll,
    noScroll,
    HoldExpr,
    withHold,
    withoutHold,
    CloseExpr,
    close,
    AllCursors,
    allCursors,
    FetchExpr,
    fetch,
    MoveExpr,
    move,
    CursorDirection,
    next,
    prior,
    first,
    last,
    absolute,
    relative,
    rowCount,
    fetchAll,
    forward,
    forwardCount,
    forwardAll,
    backward,
    backwardCount,
    backwardAll,
  )
where

import Data.Maybe (catMaybes)
import Prelude (Either, Int, Maybe (Just), either, fmap, ($), (.), (<>))

import Orville.PostgreSQL.Expr.Name (CursorName)
import Orville.PostgreSQL.Expr.Query (QueryExpr)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype DeclareExpr
  = DeclareExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

declare ::
  CursorName ->
  Maybe ScrollExpr ->
  Maybe HoldExpr ->
  QueryExpr ->
  DeclareExpr
declare cursorName maybeScrollExpr maybeHoldExpr queryExpr =
  DeclareExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "DECLARE"
        , Just $ RawSql.toRawSql cursorName
        , fmap RawSql.toRawSql maybeScrollExpr
        , Just $ RawSql.fromString "CURSOR"
        , fmap RawSql.toRawSql maybeHoldExpr
        , Just $ RawSql.fromString "FOR"
        , Just $ RawSql.toRawSql queryExpr
        ]

newtype ScrollExpr
  = ScrollExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

scroll :: ScrollExpr
scroll =
  ScrollExpr . RawSql.fromString $ "SCROLL"

noScroll :: ScrollExpr
noScroll =
  ScrollExpr . RawSql.fromString $ "NO SCROLL"

newtype HoldExpr
  = HoldExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

withHold :: HoldExpr
withHold =
  HoldExpr . RawSql.fromString $ "WITH HOLD"

withoutHold :: HoldExpr
withoutHold =
  HoldExpr . RawSql.fromString $ "WITHOUT HOLD"

newtype CloseExpr
  = CloseExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

close :: Either AllCursors CursorName -> CloseExpr
close allOrCursorName =
  CloseExpr $
    RawSql.fromString "CLOSE "
      <> either RawSql.toRawSql RawSql.toRawSql allOrCursorName

newtype AllCursors
  = AllCursors RawSql.RawSql
  deriving (RawSql.SqlExpression)

allCursors :: AllCursors
allCursors =
  AllCursors . RawSql.fromString $ "ALL"

newtype FetchExpr
  = FetchExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

fetch :: Maybe CursorDirection -> CursorName -> FetchExpr
fetch maybeDirection cursorName =
  FetchExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "FETCH"
        , fmap RawSql.toRawSql maybeDirection
        , Just $ RawSql.toRawSql cursorName
        ]

newtype MoveExpr
  = MoveExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

move :: Maybe CursorDirection -> CursorName -> MoveExpr
move maybeDirection cursorName =
  MoveExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "MOVE"
        , fmap RawSql.toRawSql maybeDirection
        , Just $ RawSql.toRawSql cursorName
        ]

newtype CursorDirection
  = CursorDirection RawSql.RawSql
  deriving (RawSql.SqlExpression)

next :: CursorDirection
next =
  CursorDirection . RawSql.fromString $ "NEXT"

prior :: CursorDirection
prior =
  CursorDirection . RawSql.fromString $ "PRIOR"

first :: CursorDirection
first =
  CursorDirection . RawSql.fromString $ "FIRST"

last :: CursorDirection
last =
  CursorDirection . RawSql.fromString $ "LAST"

absolute :: Int -> CursorDirection
absolute countParam =
  -- postgresql won't let us pass the count as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1"
  --  LINE 1: FETCH ABSOLUTE $1 \"testcursor\"
  CursorDirection $
    RawSql.fromString "ABSOLUTE "
      <> RawSql.intDecLiteral countParam

relative :: Int -> CursorDirection
relative countParam =
  CursorDirection $
    RawSql.fromString "RELATIVE "
      <>
      -- postgresql won't let us pass the count as a parameter.
      -- when we try we get an error like such error:
      --  ERROR:  syntax error at or near "$1"
      --  LINE 1: FETCH RELATIVE $1 \"testcursor\"
      RawSql.intDecLiteral countParam

rowCount :: Int -> CursorDirection
rowCount countParam =
  -- postgresql won't let us pass the count as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1"
  --  LINE 1: FETCH $1 \"testcursor\"
  CursorDirection $
    RawSql.intDecLiteral countParam

fetchAll :: CursorDirection
fetchAll =
  CursorDirection . RawSql.fromString $ "ALL"

forward :: CursorDirection
forward =
  CursorDirection . RawSql.fromString $ "FORWARD"

forwardCount :: Int -> CursorDirection
forwardCount countParam =
  -- postgresql won't let us pass the count as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1"
  --  LINE 1: FETCH FORWARD $1 \"testcursor\"
  CursorDirection $
    RawSql.fromString "FORWARD "
      <> RawSql.intDecLiteral countParam

forwardAll :: CursorDirection
forwardAll =
  CursorDirection . RawSql.fromString $ "FORWARD ALL"

backward :: CursorDirection
backward =
  CursorDirection . RawSql.fromString $ "BACKWARD"

backwardCount :: Int -> CursorDirection
backwardCount countParam =
  -- postgresql won't let us pass the count as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1"
  --  LINE 1: FETCH BACKWARD $1 \"testcursor\"
  CursorDirection $
    RawSql.fromString "BACKWARD "
      <> RawSql.intDecLiteral countParam

backwardAll :: CursorDirection
backwardAll =
  CursorDirection . RawSql.fromString $ "BACKWARD ALL"
