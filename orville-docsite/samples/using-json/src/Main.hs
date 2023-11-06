-- SNIPPET: moduleHeader
module Main
  ( main
  ) where

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson (Value, eitherDecodeStrict')
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
-- SNIPPET: dataTypes
data Foo = Foo
  { fooId :: Int.Int32
  , fooTags :: Value
  }
  deriving Show

fooIdField :: O.FieldDefinition O.NotNull Int.Int32
fooIdField =
  O.integerField "id"

fooTagsField :: O.FieldDefinition O.NotNull Value
fooTagsField =
  aesonValueField "tags"
-- SNIPPET: aesonValueField
aesonValueField :: String -> O.FieldDefinition O.NotNull Value
aesonValueField name =
  O.convertField
    (O.tryConvertSqlType encodeJSON decodeJSON)
    (O.jsonbField name)

decodeJSON :: T.Text -> Either String Value
decodeJSON =
  eitherDecodeStrict' . Enc.encodeUtf8

encodeJSON :: Value -> T.Text
encodeJSON =
  LazyText.toStrict . encodeToLazyText
-- SNIPPET: tableDefinition
fooMarshaller :: O.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> O.marshallField fooId fooIdField
    <*> O.marshallField fooTags fooTagsField

table :: O.TableDefinition (O.HasKey Int.Int32) Foo Foo
table =
  O.mkTableDefinition "json_demo" (O.primaryKey fooIdField) fooMarshaller
-- SNIPPET: mainFunction
main :: IO ()
main = do
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "host=localhost user=postgres password=postgres"
          , O.connectionNoticeReporting = O.DisableNoticeReporting
          , O.connectionPoolStripes = O.OneStripePerCapability
          , O.connectionPoolLingerTime = 10
          , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
          }

  O.runOrville pool $ do
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable table ]
    _ <- O.deleteEntity table 0
-- SNIPPET: insertEntity
    _ <- O.insertEntity table Foo { fooId = 0
                                  , fooTags = Aeson.Array $ Vector.fromList
                                      [ Aeson.Number 1
                                      , Aeson.Number 2
                                      , Aeson.Number 3
                                      ]
                                  }
    liftIO . print =<< O.findEntity table 0
-- SNIPPET: selectJSONArray
    let
      marshaller :: O.SqlMarshaller w (Int.Int32, Value)
      marshaller =
        (,) <$> O.marshallReadOnlyField fooIdField
            <*> O.marshallReadOnlyField (aesonValueField "tag")
    readEntities <-
      O.executeAndDecode
        O.SelectQuery
        (RawSql.fromString "SELECT id, jsonb_array_elements(tags) AS tag FROM json_demo")
        (Marshall.annotateSqlMarshallerEmptyAnnotation marshaller)
    liftIO $ print readEntities
