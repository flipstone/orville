{-|
Module    : Database.Orville.PostgreSQL.SqlType
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.SqlType
  (SqlType ( SqlType
           , sqlTypeDDL
           , sqlTypeReferenceDDL
           , sqlTypeNullable
           , sqlTypeId
           , sqlTypeSqlSize
           , sqlTypeToSql
           , sqlTypeFromSql
           )
  )
where

import Data.ByteString (ByteString)
import qualified Database.PostgreSQL.LibPQ as LibPQ

{-|
  SqlType defines the mapping of a Haskell type (`a`) to a SQL column type in the
  database. This includes both how to convert the type to and from the raw values
  read from the database as well as the schema information required to create
  and migrate columns using the type.
  -}
data SqlType a = SqlType
  { sqlTypeDDL :: String
    -- ^ The raw SQL DDL to use when creating/migrating columns of this type
    -- (not including any NULL or NOT NULL declarations)
  , sqlTypeReferenceDDL :: Maybe String
    -- ^ The raw SQL DDL to use when creating/migrating columns with foreign
    -- keys to this type. This is used foreignRefType to build a new SqlType
    -- when making foreign key fields
  , sqlTypeNullable :: Bool
    -- ^ Indicates whether columns should be marked NULL or NOT NULL in the
    -- database schema. If this is 'True', then 'sqlTypeFromSql' should
    -- provide a handling of 'SqlNull' that returns an 'a', not 'Nothing'.
  , sqlTypeId :: LibPQ.Oid
  , sqlTypeSqlSize :: Maybe Int
  , sqlTypeToSql :: a -> ByteString
    -- ^ A function for converting Haskell values of this type into values to
    -- be stored in the database.
  , sqlTypeFromSql :: ByteString -> Maybe a
    -- ^ A function for converting values of this are stored in the database
    -- into Haskell values. This function should return 'Nothing' to indicate
    -- an error if the conversion is impossible. Otherwise it should return
    -- 'Just' the corresponding 'a' value.
  }
