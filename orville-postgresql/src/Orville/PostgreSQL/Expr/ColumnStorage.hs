{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.ColumnStorage
  ( ColumnStorage
  , plainStorage
  , externalStorage
  , extendedStorage
  , mainStorage
  , CompressionMethod
  , compressionMethod
  , defaultCompression
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the storage mode of a column, as set by
  @ALTER COLUMN ... SET STORAGE@. E.G.

> EXTENDED

'ColumnStorage' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype ColumnStorage
  = ColumnStorage RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | The @PLAIN@ storage mode, which stores values inline, uncompressed.

@since 1.2.0.0
-}
plainStorage :: ColumnStorage
plainStorage =
  ColumnStorage (RawSql.fromString "PLAIN")

{- | The @EXTERNAL@ storage mode, which stores large values out of line,
  uncompressed.

@since 1.2.0.0
-}
externalStorage :: ColumnStorage
externalStorage =
  ColumnStorage (RawSql.fromString "EXTERNAL")

{- | The @EXTENDED@ storage mode, which compresses large values and stores
  them out of line when they are still too large.

@since 1.2.0.0
-}
extendedStorage :: ColumnStorage
extendedStorage =
  ColumnStorage (RawSql.fromString "EXTENDED")

{- | The @MAIN@ storage mode, which compresses large values and stores them
  out of line only as a last resort.

@since 1.2.0.0
-}
mainStorage :: ColumnStorage
mainStorage =
  ColumnStorage (RawSql.fromString "MAIN")

{- | Type to represent the compression method of a column, as set by
  @ALTER COLUMN ... SET COMPRESSION@. E.G.

> "lz4"

'CompressionMethod' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype CompressionMethod
  = CompressionMethod RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'CompressionMethod' naming a compression method, such as
  @pglz@ or @lz4@. The name is quoted as an identifier in the generated SQL.

@since 1.2.0.0
-}
compressionMethod :: String -> CompressionMethod
compressionMethod =
  CompressionMethod . RawSql.toRawSql . identifier

{- | The @DEFAULT@ compression method, which uses the compression method
  configured for the server.

@since 1.2.0.0
-}
defaultCompression :: CompressionMethod
defaultCompression =
  CompressionMethod (RawSql.fromString "DEFAULT")
