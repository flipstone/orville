module ParameterizedEntity.Schema.Virus
  ( virusTable
  , virusIdField
  , virusNameField
  ) where

import qualified Database.Orville.PostgreSQL as O

import ParameterizedEntity.Data.Virus (Virus(..), VirusId(..), VirusName(..))

virusTable :: O.TableDefinition (Virus VirusId) (Virus ()) VirusId
virusTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "virus"
    , O.tblPrimaryKey = O.primaryKey virusIdField
    , O.tblMapper
      -- hindent ;(
       =
        Virus <$> O.readOnlyField virusIdField <*>
        O.attrField virusName virusNameField
    , O.tblGetKey = virusId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

virusIdField :: O.FieldDefinition O.NotNull VirusId
virusIdField =
  O.automaticIdField "id" `O.withConversion`
  O.convertSqlType unVirusId VirusId

virusNameField :: O.FieldDefinition O.NotNull VirusName
virusNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType unVirusName VirusName
