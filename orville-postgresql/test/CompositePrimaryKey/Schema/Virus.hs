module CompositePrimaryKey.Schema.Virus
  ( virusTable
  , virusTypeField
  , virusSubTypeField
  , virusNameField
  ) where

import qualified Database.Orville.PostgreSQL as O

import CompositePrimaryKey.Data.Virus(
    Virus(..)
  , VirusKey
  , VirusType(..)
  , VirusSubType(..)
  , VirusName(..)
  , virusKey
  )

virusTable :: O.TableDefinition Virus Virus VirusKey
virusTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "virus"
    , O.tblPrimaryKey =
      O.compositePrimaryKey
         (O.primaryKeyPart fst virusTypeField)
         [O.primaryKeyPart snd virusSubTypeField]

    , O.tblMapper =
        Virus
        <$> O.attrField virusType virusTypeField
        <*> O.attrField virusSubType virusSubTypeField
        <*> O.attrField virusName virusNameField
    , O.tblGetKey = virusKey
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

virusTypeField :: O.FieldDefinition O.NotNull VirusType
virusTypeField =
  O.textField "type" 255 `O.withConversion`
  O.convertSqlType unVirusType VirusType

virusSubTypeField :: O.FieldDefinition O.NotNull VirusSubType
virusSubTypeField =
  O.textField "subtype" 255 `O.withConversion`
  O.convertSqlType unVirusSubType VirusSubType

virusNameField :: O.FieldDefinition O.NotNull VirusName
virusNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType unVirusName VirusName

