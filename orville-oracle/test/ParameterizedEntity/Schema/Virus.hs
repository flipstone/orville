module ParameterizedEntity.Schema.Virus
  ( virusTable
  , virusIdField
  , virusNameField
  ) where

import qualified Database.Orville.Oracle as O

import ParameterizedEntity.Data.Virus (Virus(..), VirusId(..), VirusName(..))

virusTable :: O.TableDefinition (Virus VirusId) (Virus ()) VirusId
virusTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "virus"
    , O.tblPrimaryKey = virusIdField
    , O.tblMapper
      -- hindent ;(
       =
        Virus <$> O.readOnlyField virusIdField <*>
        O.attrField virusName virusNameField
    , O.tblGetKey = virusId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

virusIdField :: O.FieldDefinition VirusId
virusIdField =
  O.automaticIdField "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType unVirusId VirusId

virusNameField :: O.FieldDefinition VirusName
virusNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType unVirusName VirusName
