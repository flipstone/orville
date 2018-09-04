{-|
Module    : Database.Orville.Internal.FromSql
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Database.Orville.Internal.FromSql where

import Control.Exception.Lifted (throw)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Convertible
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Database.HDBC

import Database.Orville.Internal.Expr
import Database.Orville.Internal.FieldDefinition
import Database.Orville.Internal.Monad
import Database.Orville.Internal.Types

convertFromSql :: Convertible SqlValue a => SqlValue -> Either FromSqlError a
convertFromSql =
  either (Left . RowDataError . prettyConvertError) Right . safeConvert

col :: (ColumnSpecifier col, Convertible SqlValue a) => col -> FromSql a
col spec = joinFromSqlError (convertFromSql <$> getColumn (selectForm spec))

fieldFromSql :: FieldDefinition a -> FromSql a
fieldFromSql field =
  joinFromSqlError (fromSqlValue <$> getColumn (selectForm field))
  where
    fromSqlValue sql =
      case fieldFromSqlValue field sql of
        Just a -> Right a
        Nothing ->
          Left $
          RowDataError $
          concat
            ["Error decoding ", show sql, " from column ", fieldName field, " value"]

class ColumnSpecifier col where
  selectForm :: col -> SelectForm

instance ColumnSpecifier SelectForm where
  selectForm = id

instance ColumnSpecifier NameForm where
  selectForm = selectColumn

instance ColumnSpecifier (FieldDefinition a) where
  selectForm = selectColumn . fromString . fieldName

instance ColumnSpecifier [Char] where
  selectForm = selectColumn . fromString

instance ColumnSpecifier T.Text where
  selectForm = selectColumn . fromString . T.unpack

instance ColumnSpecifier LT.Text where
  selectForm = selectColumn . fromString . LT.unpack

instance ColumnSpecifier BS.ByteString where
  selectForm = selectColumn . fromString . BS.unpack

instance ColumnSpecifier LBS.ByteString where
  selectForm = selectColumn . fromString . LBS.unpack

type ResultSet = [[(String, SqlValue)]]

decodeSqlRows :: FromSql result -> ResultSet -> Orville [result]
decodeSqlRows builder rows =
  fmap catMaybes $
  forM rows $ \row -> do
    case runFromSql builder row of
      Right result -> pure $ Just result
      Left err -> throw err
