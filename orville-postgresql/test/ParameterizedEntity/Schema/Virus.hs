module ParameterizedEntity.Schema.Virus
  ( virusTable
  , virusIdField
  , virusNameField
  , mutationTable
  ) where

import qualified Database.Orville.PostgreSQL as O

import ParameterizedEntity.Data.Virus (Virus(..), VirusId(..), VirusName(..), Mutation(..), MutationId(..))

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

mutationTable :: O.TableDefinition (Mutation MutationId) (Mutation ()) MutationId
mutationTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "mutation"
    , O.tblPrimaryKey = O.primaryKey mutationIdField
    , O.tblMapper = Mutation
        <$> O.readOnlyField mutationIdField
        <*> O.attrField mutationName virusNameField
        <*> O.attrField mutationParent virusIdForeignKeyField
    , O.tblGetKey = mutationId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

mutationIdField :: O.FieldDefinition O.NotNull MutationId
mutationIdField = O.automaticIdField "id" `O.withConversion`
  O.convertSqlType unMutationId MutationId

virusIdForeignKeyField :: O.FieldDefinition O.NotNull VirusId
virusIdForeignKeyField =
  O.foreignKeyFieldOnDelete O.Cascade "virusId" virusTable virusIdField
