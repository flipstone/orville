module AppManagedEntity.Schema.Virus
  ( virusTable
  , virusIdField
  , virusNameField
  ) where

import qualified Database.Orville as O

import AppManagedEntity.Data.Virus (Virus(..), VirusId(..), VirusName(..))

virusTable :: O.TableDefinition Virus Virus VirusId
virusTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "virus"
    , O.tblPrimaryKey = virusIdField
    , O.tblMapper
      -- hindent ;(
       =
        Virus <$> O.attrField virusId virusIdField <*>
        O.attrField virusName virusNameField
    , O.tblGetKey = virusId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

virusIdField :: O.FieldDefinition VirusId
virusIdField =
  O.int64Field "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType unVirusId VirusId

virusNameField :: O.FieldDefinition VirusName
virusNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType unVirusName VirusName
