module EntityWrapper.Schema.Virus
  ( virusTable
  , virusIdField
  , virusNameField
  ) where

import qualified Database.Orville.PostgreSQL as O

import EntityWrapper.Data.Entity (Entity(..))
import EntityWrapper.Data.Virus (Virus(..), VirusId(..), VirusName(..))

virusTable :: O.TableDefinition (Entity VirusId Virus) Virus VirusId
virusTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "virus"
    , O.tblPrimaryKey = virusIdField
    , O.tblMapper
      -- hindent ;(
       =
        Entity <$> O.readOnlyField virusIdField <*>
        (Virus <$> O.attrField virusName virusNameField)
    , O.tblGetKey = entityKey
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
