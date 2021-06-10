module AppManagedEntity.Schema.Virus
  ( virusTable
  , virusIdField
  , virusNameField
  ) where

import qualified Database.Orville.PostgreSQL as O

import AppManagedEntity.Data.Virus(
    Virus(..)
  , VirusDiscoveredAt(..)
  , VirusId(..)
  , VirusName(..)
  )

virusTable :: O.TableDefinition Virus Virus VirusId
virusTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "virus"
    , O.tblPrimaryKey = O.primaryKey virusIdField
    , O.tblMapper =
        Virus
        <$> O.attrField virusId virusIdField
        <*> O.attrField virusName virusNameField
        <*> O.attrField virusDiscoveredAt virusDiscoveredAtField
    , O.tblGetKey = virusId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

virusIdField :: O.FieldDefinition O.NotNull VirusId
virusIdField =
  O.int64Field "id" `O.withConversion`
  O.convertSqlType unVirusId VirusId

virusNameField :: O.FieldDefinition O.NotNull VirusName
virusNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType unVirusName VirusName

virusDiscoveredAtField :: O.FieldDefinition O.NotNull VirusDiscoveredAt
virusDiscoveredAtField =
  O.utcTimeField "discovered_at" `O.withConversion`
  O.convertSqlType unVirusDiscoveredAt VirusDiscoveredAt
