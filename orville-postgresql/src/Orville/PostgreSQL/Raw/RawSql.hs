{- |

Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

The funtions in this module are named with the intent that it is imported
qualified as 'RawSql'.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Raw.RawSql
  ( RawSql
  , parameter
  , fromString
  , fromText
  , fromBytes
  , intercalate
  , execute
  , executeVoid
  , connectionQuoting

    -- * Fragments provided for convenience
  , space
  , comma
  , commaSpace
  , leftParen
  , rightParen
  , dot
  , doubleQuote
  , doubleColon
  , stringLiteral
  , identifier
  , parenthesized

    -- * Integer values as literals
  , intDecLiteral
  , int8DecLiteral
  , int16DecLiteral
  , int32DecLiteral
  , int64DecLiteral

    -- * Generic interface for generating sql
  , SqlExpression (toRawSql, unsafeFromRawSql)
  , unsafeSqlExpression
  , toBytesAndParams
  , toExampleBytes
  , Quoting (Quoting, quoteStringLiteral, quoteIdentifier)
  , exampleQuoting
  )
where

import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity (Identity, runIdentity))
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEnc
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL.Raw.Connection as Conn
import Orville.PostgreSQL.Raw.PgTextFormatValue (PgTextFormatValue)
import Orville.PostgreSQL.Raw.SqlValue (SqlValue)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
  'RawSql' provides a type for efficiently constructing raw sql statements
  from smaller parts and then executing them. It also supports using placeholder
  values to pass parameters with a query without having to interpolate them
  as part of the actual sql state and being exposed to sql injection.

@since 1.0.0.0
-}
data RawSql
  = SqlSection BSB.Builder
  | Parameter SqlValue
  | StringLiteral BS.ByteString
  | Identifier BS.ByteString
  | Append RawSql RawSql

instance Semigroup RawSql where
  (SqlSection builderA) <> (SqlSection builderB) =
    SqlSection (builderA <> builderB)
  otherA <> otherB =
    Append otherA otherB

instance Monoid RawSql where
  mempty = SqlSection mempty

{- |
 'SqlExpression' provides a common interface for converting types to and from
 'RawSql', either via 'toRawSql' and 'unsafeFromRawSql', or the convenience
 function 'unsafeSqlExpression'. Orville defines a large number of types that
 represent various fragments of SQL statements as well as functions to help
 construct the safely. These funtions can be found in the
 'Orville.PostgreSQL.Expr'. These types all provide 'SqlExpression' instances
 as an escape hatch to allow you to pass any SQL you wish in place of what
 Orville directly supports. This should be use with great care as Orville
 cannot guarantee that the SQL you pass can be used to generate valid SQL in
 conjuction with the rest of the 'Orville.PostgreSQL.Expr' API.

@since 1.0.0.0
-}
class SqlExpression a where
  toRawSql :: a -> RawSql
  unsafeFromRawSql :: RawSql -> a

instance SqlExpression RawSql where
  toRawSql = id
  unsafeFromRawSql = id

{- |
A conveinence function for creating an arbitrary 'SqlExpression' from a
'String'. Great care should be exercised in use of this function as it cannot
provide any sort of guarantee that the string passed is usable to generate
valid SQL via the rest of Orville's 'Orville.PostgreSQL.Expr' API as the
whatever 'SqlExpression' type is returned.

For example, if one wanted build a boolean expression not support by Orville,
you can do it like so

> import qualified Orville.PostgreSQL.Expr as Expr
>
> a :: Expr.BooleanExpr
> a RawSql.unsafeSqlExpression "foo BETWEEN 1  AND 3"
@since 1.0.0.0
-}
unsafeSqlExpression :: SqlExpression a => String -> a
unsafeSqlExpression =
  unsafeFromRawSql . fromString

{- |
  Provides procedures for quoting parts of a raw SQL query so that they can be
  safely executed. Quoting may be done in some 'Monad' m, allowing for the use
  of quoting operations provided by 'Conn.Connection', which operates in the
  'IO' monad.

  See 'connectionQuoting' and 'exampleQuoting'.

@since 1.0.0.0
-}
data Quoting m = Quoting
  { quoteStringLiteral :: BS.ByteString -> m BSB.Builder
  , quoteIdentifier :: BS.ByteString -> m BSB.Builder
  }

{- |
  Quoting done in pure Haskell that is suitable for showing SQL examples,
  but is not guaranteed to be sufficient for all database connections. For
  quoting that is based on the actual connection to the database, see
  'connectionQuoting'.

@since 1.0.0.0
-}
exampleQuoting :: Quoting Identity
exampleQuoting =
  Quoting
    { quoteStringLiteral = Identity . exampleQuoteString '\''
    , quoteIdentifier = Identity . exampleQuoteString '"'
    }

exampleQuoteString :: Char -> BS.ByteString -> BSB.Builder
exampleQuoteString quoteChar =
  let
    quote (Right bs) =
      case B8.uncons bs of
        Nothing ->
          Nothing
        Just (char, rest) ->
          Just $
            if char == quoteChar
              then (char, Left (char, rest))
              else (char, Right rest)
    quote (Left (char, rest)) =
      Just (char, Right rest)

    quoteBytes =
      BSB.char8 quoteChar
  in
    \unquoted ->
      quoteBytes
        <> BSB.byteString (B8.unfoldr quote (Right unquoted))
        <> quoteBytes

{- |
  Quoting done in IO based using the quoting functions provided by the
  connection, which can apply quoting based on the specific connection
  properties.

  If you don't have a connection available and are only planning on using
  the SQL for explanatory or example purposes, see 'exampleQuoting'.

@since 1.0.0.0
-}
connectionQuoting :: Conn.Connection -> Quoting IO
connectionQuoting connection =
  Quoting
    { quoteStringLiteral = Conn.quoteStringLiteral connection
    , quoteIdentifier = Conn.quoteIdentifier connection
    }

{- |
  Constructs the actual SQL bytestring and parameter values that will be
  passed to the database to execute a 'RawSql' query. Any string
  literals thar are included in the SQL expression will be quoting
  using the given quoting directive.

@since 1.0.0.0
-}
toBytesAndParams ::
  (SqlExpression sql, Monad m) =>
  Quoting m ->
  sql ->
  m (BS.ByteString, [Maybe PgTextFormatValue])
toBytesAndParams quoting sql = do
  (byteBuilder, finalProgress) <-
    buildSqlWithProgress quoting startingProgress (toRawSql sql)
  pure
    ( LBS.toStrict (BSB.toLazyByteString byteBuilder)
    , DList.toList (paramValues finalProgress)
    )

{- |
  Builds the bytes that represent the raw sql. These bytes may not be executable
  on their own, because they may contain placeholders that must be filled in,
  but can be useful for inspecting sql queries.

@since 1.0.0.0
-}
toExampleBytes :: SqlExpression sql => sql -> BS.ByteString
toExampleBytes =
  fst . runIdentity . toBytesAndParams exampleQuoting

{- |
  This is an internal datatype used during the sql building process to track
  how many params have been seen so that placeholder indices (e.g. '$1', etc)
  can be generated to include in the sql.

@since 1.0.0.0
-}
data ParamsProgress = ParamsProgress
  { paramCount :: Int
  , paramValues :: DList (Maybe PgTextFormatValue)
  }

{- |
  An initial value for 'ParamsProgress' that indicates no params have been been
  encountered yet.

@since 1.0.0.0
-}
startingProgress :: ParamsProgress
startingProgress =
  ParamsProgress
    { paramCount = 0
    , paramValues = DList.empty
    }

{- |
  Adds a parameter value to the end of the params list, tracking the count
  of parameters as it does so.

@since 1.0.0.0
-}
snocParam :: ParamsProgress -> Maybe PgTextFormatValue -> ParamsProgress
snocParam (ParamsProgress count values) newValue =
  ParamsProgress
    { paramCount = count + 1
    , paramValues = DList.snoc values newValue
    }

{- |
  Constructs a bytestring builder that can be executed to get the bytes for a
  section of 'RawSql'. This function takes and returns a 'ParamsProgress' so
  that placeholder indices (e.g. '$1') and their corresponding parameter values
  can be tracked across multiple sections of raw sql.

@since 1.0.0.0
-}
buildSqlWithProgress ::
  Monad m =>
  Quoting m ->
  ParamsProgress ->
  RawSql ->
  m (BSB.Builder, ParamsProgress)
buildSqlWithProgress quoting progress rawSql =
  case rawSql of
    SqlSection builder ->
      pure (builder, progress)
    StringLiteral unquotedString -> do
      quotedString <- quoteStringLiteral quoting unquotedString
      pure (quotedString, progress)
    Identifier unquotedIdentifier -> do
      quotedIdentifier <- quoteIdentifier quoting unquotedIdentifier
      pure (quotedIdentifier, progress)
    Parameter value ->
      let
        newProgress = snocParam progress (SqlValue.toPgValue value)
        placeholder = BSB.stringUtf8 "$" <> BSB.intDec (paramCount newProgress)
      in
        pure (placeholder, newProgress)
    Append first second -> do
      (firstBuilder, nextProgress) <- buildSqlWithProgress quoting progress first
      (secondBuilder, finalProgress) <- buildSqlWithProgress quoting nextProgress second
      pure (firstBuilder <> secondBuilder, finalProgress)

{- |
  Constructs a 'RawSql' from a 'String' value using utf8 encoding.

  Note that because the string is treated as raw sql it completely up to the
  caller to protected againt sql-injections attacks when using this function.
  Never use this function with input read from an untrusted source.

@since 1.0.0.0
-}
fromString :: String -> RawSql
fromString =
  SqlSection . BSB.stringUtf8

{- |
  Constructs a 'RawSql' from a 'Text' value using utf8 encoding.

  Note that because the text is treated as raw sql it completely up to the
  caller to protected againt sql-injections attacks when using this function.
  Never use this function with input read from an untrusted source.

@since 1.0.0.0
-}
fromText :: T.Text -> RawSql
fromText =
  SqlSection . TextEnc.encodeUtf8Builder

{- |
  Constructs a 'RawSql' from a 'ByteString' value, which is assumed to be
  encoded sensibly for the database to handle.

  Note that because the string is treated as raw sql it completely up to the
  caller to protected againt sql-injections attacks when using this function.
  Never use this function with input read from an untrusted source.

@since 1.0.0.0
-}
fromBytes :: BS.ByteString -> RawSql
fromBytes =
  SqlSection . BSB.byteString

{- |
  Includes an input parameter in the 'RawSql' statement that will be passed
  using placeholders (e.g. '$1') rather than being included directly in the sql
  statement. This is the correct way to include input from untrusted sources as
  part of a 'RawSql' query. The parameter must be formatted in a textual
  representation, which the database will interpret. The database type for the
  value will be inferred by the database based on its usage in the query.

@since 1.0.0.0
-}
parameter :: SqlValue -> RawSql
parameter =
  Parameter

{- |
  Includes a bytestring value as string literal in the SQL statement. The
  string literal will be quoted and escaped for you, the value provided should
  not include surrounding quotes or quote special characters.

  Note: It's better to use the 'parameter' function where possible to pass
  values to be used as input to a SQL statement. There are some situations
  where PostgreSQL does not allow this, however (for instance, in some DDL
  statements). This function is provided for those situations.

@since 1.0.0.0
-}
stringLiteral :: BS.ByteString -> RawSql
stringLiteral =
  StringLiteral

{- |
  Includes a bytestring value as an identifier in the SQL statement. The
  identifier will be quoted and escaped for you, the value provided should not
  include surrounding quotes or quote special characters.

@since 1.0.0.0
-}
identifier :: BS.ByteString -> RawSql
identifier =
  Identifier

{- |
  Concatenates a list of 'RawSql' values using another 'RawSql' value as
  the a separator between the items.

@since 1.0.0.0
-}
intercalate :: (SqlExpression sql, Foldable f) => RawSql -> f sql -> RawSql
intercalate separator =
  mconcat
    . List.intersperse separator
    . map toRawSql
    . Fold.toList

{- |
  Executes a 'RawSql' value using the 'Conn.executeRaw' function. Make sure
  to read the documentation of 'Conn.executeRaw' for caveats and warnings.
  Use with caution.

  Note that because this is done in 'IO' no callback functions are available to
  be called.

@since 1.0.0.0
-}
execute :: SqlExpression sql => Conn.Connection -> sql -> IO LibPQ.Result
execute connection sql = do
  (sqlBytes, params) <- toBytesAndParams (connectionQuoting connection) sql
  Conn.executeRaw connection sqlBytes params

{- |
  Executes a 'RawSql' value using the 'Conn.executeRawVoid' function. Make sure
  to read the documentation of 'Conn.executeRawVoid' for caveats and warnings.
  Use with caution.

  Note that because this is done in 'IO' no callback functions are available to
  be called.

@since 1.0.0.0
-}
executeVoid :: SqlExpression sql => Conn.Connection -> sql -> IO ()
executeVoid connection sql = do
  void $ execute connection sql

-- | Just a plain old space, provided for convenience
space :: RawSql
space = fromString " "

-- | Just a plain old comma, provided for convenience
comma :: RawSql
comma = fromString ","

-- | Comma space separator, provided for convenience
commaSpace :: RawSql
commaSpace = fromString ", "

-- | Just a plain old left paren, provided for convenience
leftParen :: RawSql
leftParen = fromString "("

-- | Just a plain old right paren, provided for convenience
rightParen :: RawSql
rightParen = fromString ")"

-- | Just a plain period, provided for convenience
dot :: RawSql
dot = fromString "."

-- | Just a plain double quote, provided for convenience
doubleQuote :: RawSql
doubleQuote = fromString "\""

-- | Just two colons, provided for convenience
doubleColon :: RawSql
doubleColon = fromString "::"

{- |
  Constructs a 'RawSql' from an 'Int.Int8' value. The integral value is included
  directly in the SQL string, not passed as a parameter. When dealing with user
  input it is better to use 'parameter' rather whenever possible.

@since 1.0.0.0
-}
int8DecLiteral :: Int.Int8 -> RawSql
int8DecLiteral =
  SqlSection . BSB.int8Dec

{- |
  Constructs a 'RawSql' from an 'Int.Int16' value. The integral value is included
  directly in the SQL string, not passed as a parameter. When dealing with user
  input it is better to use 'parameter' rather whenever possible.

@since 1.0.0.0
-}
int16DecLiteral :: Int.Int16 -> RawSql
int16DecLiteral =
  SqlSection . BSB.int16Dec

{- |
  Constructs a 'RawSql' from an 'Int.Int32' value. The integral value is included
  directly in the SQL string, not passed as a parameter. When dealing with user
  input it is better to use 'parameter' rather whenever possible.

@since 1.0.0.0
-}
int32DecLiteral :: Int.Int32 -> RawSql
int32DecLiteral =
  SqlSection . BSB.int32Dec

{- |
  Constructs a 'RawSql' from an 'Int.Int64' value. The integral value is included
  directly in the SQL string, not passed as a parameter. When dealing with user
  input it is better to use 'parameter' rather whenever possible.

@since 1.0.0.0
-}
int64DecLiteral :: Int.Int64 -> RawSql
int64DecLiteral =
  SqlSection . BSB.int64Dec

{- |
  Constructs a 'RawSql' from an 'Int' value. The integral value is included
  directly in the SQL string, not passed as a parameter. When dealing with user
  input it is better to use 'parameter' rather whenever possible.

@since 1.0.0.0
-}
intDecLiteral :: Int -> RawSql
intDecLiteral =
  SqlSection . BSB.intDec

{- |
  Constructs a 'RawSql' by putting parentheses around an arbitrary expression.
  The result is returned as a 'RawSql'. It is up to the caller to decide whether
  it should be wrapped in a more specific expression type.

@since 1.0.0.0
-}
parenthesized :: SqlExpression sql => sql -> RawSql
parenthesized expr =
  leftParen <> toRawSql expr <> rightParen
