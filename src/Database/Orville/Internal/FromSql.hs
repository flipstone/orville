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

convertFromSql :: Convertible SqlValue a => SqlValue -> FromSql a
convertFromSql sqlValue =
  case safeConvert sqlValue of
    Right value -> return value
    Left conversionError -> throwError $ RowDataError $ prettyConvertError conversionError

nextColumn :: Convertible SqlValue a => FromSql a
nextColumn = do
  columns <- get

  case columns of
    [] -> throwError $ QueryError $ "Insufficient column values to build type"
    (_,sqlValue) : rest -> do
      put rest
      convertFromSql sqlValue

columnPrefix :: FromSql String
columnPrefix = ask

prefixed :: String -> FromSql a -> FromSql a
prefixed str = local (++str)

fullColumnName :: ColumnSpecifier col => col -> FromSql String
fullColumnName colSpec = do
  prefix <- columnPrefix
  pure $ prefix ++ columnName colSpec

col :: (ColumnSpecifier col, Convertible SqlValue a)
    => col -> FromSql a
col colSpec = do
  columns <- get
  name <- fullColumnName colSpec

  case lookup name columns of
    Just sqlValue -> convertFromSql sqlValue
    Nothing ->
      throwError $ QueryError $ concat [ "Column "
                                       , name
                                       , " not found in result set, "
                                       , " actual columns: "
                                       , intercalate "," $ map fst columns
                                       ]

-- allows a Nothing to be returned for a (Maybe field) within a (Maybe data_type)
nullableCol :: (ColumnSpecifier col, Convertible SqlValue a)
         => col
         -> FromSql (Maybe (Maybe a))
nullableCol colSpec = (col colSpec) >>= pure . Just

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

