{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Orville.Internal.FromSql where

import            Control.Monad.Except
import            Control.Monad.Reader
import            Control.Monad.State
import qualified  Data.ByteString.Char8 as BS
import qualified  Data.ByteString.Lazy.Char8 as LBS
import            Data.Convertible
import            Data.List
import qualified  Data.Text as T
import qualified  Data.Text.Lazy as LT
import            Database.HDBC

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types

convertFromSql :: Convertible SqlValue a => SqlValue -> Either FromSqlError a
convertFromSql =
  either (Left . RowDataError . prettyConvertError) Right . safeConvert

col :: (ColumnSpecifier col, Convertible SqlValue a) => col -> FromSql a
col colSpec = joinFromSqlError (convertFromSql <$> getColumn (columnName colSpec))

class ColumnSpecifier col where
  columnName :: col -> String

instance ColumnSpecifier FieldDefinition where
  columnName = fieldName

instance ColumnSpecifier [Char] where
  columnName = id

instance ColumnSpecifier T.Text where
  columnName = T.unpack

instance ColumnSpecifier LT.Text where
  columnName = LT.unpack

instance ColumnSpecifier BS.ByteString where
  columnName = BS.unpack

instance ColumnSpecifier LBS.ByteString where
  columnName = LBS.unpack

