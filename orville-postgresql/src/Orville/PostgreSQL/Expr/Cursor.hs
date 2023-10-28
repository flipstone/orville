{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Cursor
  ( DeclareExpr
  , declare
  , ScrollExpr
  , scroll
  , noScroll
  , HoldExpr
  , withHold
  , withoutHold
  , CloseExpr
  , close
  , AllCursors
  , allCursors
  , FetchExpr
  , fetch
  , MoveExpr
  , move
  , CursorDirection
  , next
  , prior
  , first
  , last
  , absolute
  , relative
  , rowCount
  , fetchAll
  , forward
  , forwardCount
  , forwardAll
  , backward
  , backwardCount
  , backwardAll
  )
where

import Data.Maybe (catMaybes)
import Prelude (Either, Int, Maybe (Just), either, fmap, ($), (.), (<>))

import Orville.PostgreSQL.Expr.Name (CursorName)
import Orville.PostgreSQL.Expr.Query (QueryExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
'DeclareExpr' corresponds to the SQL DECLARE statement, for declaring and
opening cursors. E.G.

> DECLARE FOO CURSOR FOR SELECT * FROM BAR

See PostgreSQL [cursor declare
documentation](https://www.postgresql.org/docs/current/sql-declare.html) for
more information.

'DeclareExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype DeclareExpr
  = DeclareExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | A smart constructor for setting up a 'DeclareExpr'. This, along with other functions provided,
   allows users to more safely declare a cursor.

@since 1.0.0.0
-}
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

{- |
'ScrollExpr' is used to determine if a cursor should be able to fetch
nonsequentially. E.G.

> NO SCROLL

Note that the default in at least PostgreSQL versions 11-15 is to allow
nonsequential fetches under some, but not all, circumstances.

See PostgreSQL [cursor declare
documentation](https://www.postgresql.org/docs/current/sql-declare.html) for more information.

'ScrollExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ScrollExpr
  = ScrollExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Allow a cursor to be used to fetch rows nonsequentially.

@since 1.0.0.0
-}
scroll :: ScrollExpr
scroll =
  ScrollExpr . RawSql.fromString $ "SCROLL"

{- | Only allow a cursor to be used to fetch rows sequentially.

@since 1.0.0.0
-}
noScroll :: ScrollExpr
noScroll =
  ScrollExpr . RawSql.fromString $ "NO SCROLL"

{- |
'HoldExpr' is used to determine if a cursor should be available for use after
the transaction that created it has been committed. E.G.

> WITH HOLD

See PostgreSQL [cursor documentation](https://www.postgresql.org/docs/current/sql-declare.html) for
more information.

'HoldExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype HoldExpr
  = HoldExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Allow a cursor to be used after the transaction creating it is committed.

@since 1.0.0.0
-}
withHold :: HoldExpr
withHold =
  HoldExpr . RawSql.fromString $ "WITH HOLD"

{- | Do not allow a cursor to be used after the transaction creating it is committed.

@since 1.0.0.0
-}
withoutHold :: HoldExpr
withoutHold =
  HoldExpr . RawSql.fromString $ "WITHOUT HOLD"

{- |
'CloseExpr' corresponds to the SQL CLOSE statement. E.G.

> CLOSE ALL

See PostgreSQL [close documentation](https://www.postgresql.org/docs/current/sql-close.html) for
more information.

'HoldExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype CloseExpr
  = CloseExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | A smart constructor for setting up a 'CloseExpr', either closing all cursors or the given named
   cursor.

@since 1.0.0.0
-}
close :: Either AllCursors CursorName -> CloseExpr
close allOrCursorName =
  CloseExpr $
    RawSql.fromString "CLOSE "
      <> either RawSql.toRawSql RawSql.toRawSql allOrCursorName

{- |
'AllCursors' corresponds to the ALL keyword in a CLOSE statement. E.G.

> ALL

'AllCursors' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype AllCursors
  = AllCursors RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Specify closing all open cursors, for use with a 'CloseExpr'.

@since 1.0.0.0
-}
allCursors :: AllCursors
allCursors =
  AllCursors . RawSql.fromString $ "ALL"

{- |
'FetchExpr' corresponds to the SQL FETCH statement, for retrieving rows from a
previously-created cursor. E.G.

> FETCH NEXT FOO

See PostgreSQL [fetch
documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for more
information.

'FetchExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype FetchExpr
  = FetchExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Construct a 'FetchExpr', for a given cursor and optionally a direction to fetch.

@since 1.0.0.0
-}
fetch :: Maybe CursorDirection -> CursorName -> FetchExpr
fetch maybeDirection cursorName =
  FetchExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "FETCH"
        , fmap RawSql.toRawSql maybeDirection
        , Just $ RawSql.toRawSql cursorName
        ]

{- |
'MoveExpr' corresponds to the SQL MOVE statement, for positioning a previously
created cursor, /without/ retrieving any rows. E.G.

> MOVE NEXT FOO

'MoveExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype MoveExpr
  = MoveExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Construct a 'MoveExpr', for a given cursor and optionally a direction to move.

@since 1.0.0.0
-}
move :: Maybe CursorDirection -> CursorName -> MoveExpr
move maybeDirection cursorName =
  MoveExpr
    . RawSql.intercalate RawSql.space
    $ catMaybes
      [ Just $ RawSql.fromString "MOVE"
      , fmap RawSql.toRawSql maybeDirection
      , Just $ RawSql.toRawSql cursorName
      ]

{- |
'CursorDirection' corresponds to the direction argument to the SQL FETCH and
MOVE statements. E.G.

> BACKWARD

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

'CursorDirection' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype CursorDirection
  = CursorDirection RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Specify the direction of the next single row. Primarily for use with
    'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
next :: CursorDirection
next =
  CursorDirection . RawSql.fromString $ "NEXT"

{- | Specify the direction of the prior single row. Primarily for use with
    'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
prior :: CursorDirection
prior =
  CursorDirection . RawSql.fromString $ "PRIOR"

{- | Specify the direction of the first single row. Primarily for use with
    'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
first :: CursorDirection
first =
  CursorDirection . RawSql.fromString $ "FIRST"

{- | Specify the direction of the last single row. Primarily for use with
    'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
last :: CursorDirection
last =
  CursorDirection . RawSql.fromString $ "LAST"

{- | Specify the direction of the single row at an absolute position within the
    cursor. Primarily for use with 'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
absolute :: Int -> CursorDirection
absolute countParam =
  -- postgresql won't let us pass the count as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1"
  --  LINE 1: FETCH ABSOLUTE $1 \"testcursor\"
  CursorDirection $
    RawSql.fromString "ABSOLUTE "
      <> RawSql.intDecLiteral countParam

{- | Specify the direction of the single row relative to the cursor's current
    position. Primarily for use with 'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
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

{- | Specify the direction of the next n rows. Primarily for use with 'fetch'
    or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
rowCount :: Int -> CursorDirection
rowCount countParam =
  -- postgresql won't let us pass the count as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1"
  --  LINE 1: FETCH $1 \"testcursor\"
  CursorDirection $
    RawSql.intDecLiteral countParam

{- | Specify the direction of all the next rows. Primarily for use with 'fetch'
    or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
fetchAll :: CursorDirection
fetchAll =
  CursorDirection . RawSql.fromString $ "ALL"

{- | Specify the direction of the next single row. Primarily for use with
    'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
forward :: CursorDirection
forward =
  CursorDirection . RawSql.fromString $ "FORWARD"

{- | Specify the direction of the next n rows. Primarily for use with 'fetch'
    or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
forwardCount :: Int -> CursorDirection
forwardCount countParam =
  -- postgresql won't let us pass the count as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1"
  --  LINE 1: FETCH FORWARD $1 \"testcursor\"
  CursorDirection $
    RawSql.fromString "FORWARD "
      <> RawSql.intDecLiteral countParam

{- | Specify the direction of all the next rows. Primarily for use with 'fetch'
    or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
forwardAll :: CursorDirection
forwardAll =
  CursorDirection . RawSql.fromString $ "FORWARD ALL"

{- | Specify the direction of the prior single row. Primarily for use with
    'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
backward :: CursorDirection
backward =
  CursorDirection . RawSql.fromString $ "BACKWARD"

{- | Specify the direction of the prior n rows. Primarily for use with 'fetch'
    or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
backwardCount :: Int -> CursorDirection
backwardCount countParam =
  -- postgresql won't let us pass the count as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1"
  --  LINE 1: FETCH BACKWARD $1 \"testcursor\"
  CursorDirection $
    RawSql.fromString "BACKWARD "
      <> RawSql.intDecLiteral countParam

{- | Specify the direction of all the prior rows. Primarily for use with
    'fetch' or 'move'.

See PostgreSQL [fetch documentation](https://www.postgresql.org/docs/current/sql-fetch.html) for
more information.

@since 1.0.0.0
-}
backwardAll :: CursorDirection
backwardAll =
  CursorDirection . RawSql.fromString $ "BACKWARD ALL"
