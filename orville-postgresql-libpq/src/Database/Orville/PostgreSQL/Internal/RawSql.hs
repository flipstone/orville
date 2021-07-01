{- |

Module    : Database.Orville.PostgreSQL.Internal.RawSql
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT

The funtions in this module are named with the intent that it is imported
qualified as 'RawSql'.
-}
module Database.Orville.PostgreSQL.Internal.RawSql
  ( RawSql,
    parameter,
    fromString,
    fromBytes,
    intercalate,
    execute,
    executeVoid,

    -- * Fragmants provided for convenience
    space,
    comma,
    leftParen,
    rightParen,

    -- * Generic interface for generating sql
    ToRawSql (toRawSql),
    toBytesAndParams,
    toBytes,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Database.Orville.PostgreSQL.Connection (Connection)
import qualified Database.Orville.PostgreSQL.Connection as Conn
import Database.Orville.PostgreSQL.Internal.PGTextFormatValue (PGTextFormatValue)
import Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

{- |
  'RawSql' provides a type for efficiently constructing raw sql statements
  from smaller parts and then executing them. It also supports using placeholder
  values to pass parameters with a query without having to interpolate them
  as part of the actual sql state and being exposed to sql injection.
-}
data RawSql
  = SqlSection BSB.Builder
  | Parameter SqlValue
  | Append RawSql RawSql

instance Semigroup RawSql where
  (SqlSection builderA) <> (SqlSection builderB) =
    SqlSection (builderA <> builderB)
  otherA <> otherB =
    Append otherA otherB

instance Monoid RawSql where
  mempty = SqlSection mempty

class ToRawSql a where
  toRawSql :: a -> RawSql

instance ToRawSql RawSql where
  toRawSql = id

{- |
  Constructs the actual sql bytestring and parameter values that will be
  passed to the database to execute a 'RawSql' query.
-}
toBytesAndParams :: ToRawSql sql => sql -> (BS.ByteString, [Maybe PGTextFormatValue])
toBytesAndParams sql =
  let (byteBuilder, finalProgress) =
        buildSqlWithProgress startingProgress (toRawSql sql)
   in ( LBS.toStrict (BSB.toLazyByteString byteBuilder)
      , DList.toList (paramValues finalProgress)
      )

{- |
  Builds te bytes that represent the raw sql. These bytes may not be executable
  on their own, because they may contain placeholders that must be filled in,
  but can be useful for inspecting sql queries.
-}
toBytes :: ToRawSql sql => sql -> BS.ByteString
toBytes =
  fst . toBytesAndParams

{- |
  This is an internal datatype used during the sql building process to track
  how many params have been seen so that placeholder indices (e.g. '$1', etc)
  can be generated to include in the sql.
-}
data ParamsProgress = ParamsProgress
  { paramCount :: Int
  , paramValues :: DList (Maybe PGTextFormatValue)
  }

{- |
  An initial value for 'ParamsProgress' that indicates no params have been been
  encountered yet.
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
-}
snocParam :: ParamsProgress -> Maybe PGTextFormatValue -> ParamsProgress
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
-}
buildSqlWithProgress ::
  ParamsProgress ->
  RawSql ->
  (BSB.Builder, ParamsProgress)
buildSqlWithProgress progress rawSql =
  case rawSql of
    SqlSection builder ->
      (builder, progress)
    Parameter value ->
      let newProgress = snocParam progress (SqlValue.toPGValue value)
       in ( BSB.stringUtf8 "$" <> BSB.intDec (paramCount newProgress)
          , newProgress
          )
    Append first second ->
      let (firstBuilder, nextProgress) = buildSqlWithProgress progress first
          (secondBuilder, finalProgress) = buildSqlWithProgress nextProgress second
       in (firstBuilder <> secondBuilder, finalProgress)

{- |
  Constructs a 'RawSql' from a 'String' value using utf8 encoding.

  Note that because the string is treated as raw sql it completely up to the
  caller to protected againt sql-injections attacks when using this function.
  Never use this function with input read from an untrusted source.
-}
fromString :: String -> RawSql
fromString =
  SqlSection . BSB.stringUtf8

{- |
  Constructs a 'RawSql' from a 'ByteString' value, which is assumed to be
  encoded sensibly for the database to handle.

  Note that because the string is treated as raw sql it completely up to the
  caller to protected againt sql-injections attacks when using this function.
  Never use this function with input read from an untrusted source.
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
-}
parameter :: SqlValue -> RawSql
parameter =
  Parameter

{- |
  Concatenates a list of 'RawSql' values using another 'RawSql' value as
  the a separator between the items.
-}
intercalate :: RawSql -> [RawSql] -> RawSql
intercalate separator parts =
  mconcat (List.intersperse separator parts)

{- |
  Executes a 'RawSql' value using the 'Conn.executeRaw' function. Make sure
  to read the documentation of 'Conn.executeRaw' for caveats and warnings.
  Use with caution.
-}
execute :: ToRawSql sql => Connection -> sql -> IO LibPQ.Result
execute connection sql =
  let (sqlBytes, params) =
        toBytesAndParams sql
   in Conn.executeRaw connection sqlBytes params

{- |
  Executes a 'RawSql' value using the 'Conn.executeRawVoid' function. Make sure
  to read the documentation of 'Conn.executeRawVoid' for caveats and warnings.
  Use with caution.
-}
executeVoid :: ToRawSql sql => Connection -> sql -> IO ()
executeVoid connection sql =
  let (sqlBytes, params) =
        toBytesAndParams sql
   in Conn.executeRawVoid connection sqlBytes params

-- | Just a plain old space, provided for convenience
space :: RawSql
space = fromString " "

-- | Just a plain old comma, provided for convenience
comma :: RawSql
comma = fromString ","

-- | Just a plain old left paren, provided for convenience
leftParen :: RawSql
leftParen = fromString "("

-- | Just a plain old right paren, provided for convenience
rightParen :: RawSql
rightParen = fromString ")"
