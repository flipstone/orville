module Test.InformationSchema
  ( informationSchemaTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
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
                      ( Orville.fieldEquals InformationSchema.tableSchemaField (T.pack "information_schema")
                          :| [ Orville.fieldEquals InformationSchema.tableNameField (T.pack "tables")
                             ]
                      )
                )

            let expected =
                  InformationSchema.InformationSchemaTable
                    { InformationSchema.tableCatalog = T.pack "orville_test"
                    , InformationSchema.tableSchema = T.pack "information_schema"
                    , InformationSchema.tableName = T.pack "tables"
                    }

            result HH.=== Just expected
        )
      ]
