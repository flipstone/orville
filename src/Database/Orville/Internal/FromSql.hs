{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Database.Orville.Internal.FromSql where

import            Control.Exception.Lifted (throw)
import            Control.Monad
import            Control.Monad.IO.Class
import            Data.Maybe
import qualified  Data.ByteString.Char8 as BS
import qualified  Data.ByteString.Lazy.Char8 as LBS
import            Data.Convertible
import qualified  Data.Text as T
import qualified  Data.Text.Lazy as LT
import            Database.HDBC

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Monad
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

type ResultSet = [[(String, SqlValue)]]

decodeSqlRows :: FromSql result -> ResultSet -> Orville [result]
decodeSqlRows builder rows =
  fmap catMaybes $ forM rows $ \row -> do
    case runFromSql builder row of
      Right result -> pure $ Just result

      (Left (RowDataError msg)) -> do
        liftIO $ putStrLn $ concat
          [ "** Warning ** Error converting row from sql: "
          , show msg
          , ". First column was was: "
          , maybe "<no columns present>" show (listToMaybe row)
          ]

        pure Nothing

      Left err -> throw err

