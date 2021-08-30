module Test.InformationSchema
  ( informationSchemaTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.InformationSchema as InformationSchema

import qualified Test.Property as Property

informationSchemaTests :: Pool.Pool Conn.Connection -> IO Bool
informationSchemaTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "InformationSchema")
      [
        ( String.fromString "Can query information about a tables"
        , Property.singletonProperty $ do
            result <- MIO.liftIO . Orville.runOrville pool $ do
              Orville.findFirstEntityBy
                InformationSchema.informationSchemaTablesTable
                ( Orville.where_ $
                    Orville.whereAnd
                      ( Orville.fieldEquals InformationSchema.tableSchemaField informationSchemaSchemaName
                          :| [ Orville.fieldEquals InformationSchema.tableNameField tablesTableName
                             ]
                      )
                )

            let expected =
                  InformationSchema.InformationSchemaTable
                    { InformationSchema.tableCatalog = orvilleTestCatalogName
                    , InformationSchema.tableSchema = informationSchemaSchemaName
                    , InformationSchema.tableName = tablesTableName
                    }

            result HH.=== Just expected
        )
      ]

orvilleTestCatalogName :: InformationSchema.CatalogName
orvilleTestCatalogName =
  String.fromString "orville_test"

informationSchemaSchemaName :: InformationSchema.SchemaName
informationSchemaSchemaName =
  String.fromString "information_schema"

tablesTableName :: InformationSchema.TableName
tablesTableName =
  String.fromString "tables"
