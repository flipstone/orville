module Orville.PostgreSQL.PgCatalog.DatabaseDescription
  ( DatabaseDescription (..),
    RelationDescription (..),
    lookupRelation,
    lookupAttribute,
    describeDatabaseRelations,
  )
where

import qualified Data.Map.Strict as Map
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.PgAttribute (AttributeName, PgAttribute (pgAttributeName), attributeIsDroppedField, pgAttributeTable, relationOidField)
import Orville.PostgreSQL.PgCatalog.PgClass (PgClass (pgClassOid), RelationName, namespaceOidField, pgClassTable, relationNameField)
import Orville.PostgreSQL.PgCatalog.PgNamespace (NamespaceName, PgNamespace (pgNamespaceOid), namespaceNameField, pgNamespaceTable)
import qualified Orville.PostgreSQL.Plan as Plan
import qualified Orville.PostgreSQL.Plan.Many as Many
import qualified Orville.PostgreSQL.Plan.Operation as Op

{- |
  A description of selected items from a single PostgreSQL database.
  'describeDatabaseRelations' can be used to load the descriptions of request
  items.
-}
data DatabaseDescription = DatabaseDescription
  { databaseRelations :: Map.Map (NamespaceName, RelationName) RelationDescription
  }

lookupRelation ::
  (NamespaceName, RelationName) ->
  DatabaseDescription ->
  Maybe RelationDescription
lookupRelation key =
  Map.lookup key . databaseRelations

{- |
  A description of a particular relation in the PostgreSQL database, including
  the attributes of the relation.
-}
data RelationDescription = RelationDescription
  { relationRecord :: PgClass
  , relationAttributes :: Map.Map AttributeName PgAttribute
  }

lookupAttribute ::
  AttributeName ->
  RelationDescription ->
  Maybe PgAttribute
lookupAttribute key =
  Map.lookup key . relationAttributes

{- |
  Describes the requested relations in the current database. If any of the
  relations do not exist, they will not have an entry in the returned
  description.

  Each 'RelationDescription' will contain all the attributes that currently
  exist for that relation, according to the @pg_catalog@ tables.
-}
describeDatabaseRelations ::
  Orville.MonadOrville m =>
  [(NamespaceName, RelationName)] ->
  m DatabaseDescription
describeDatabaseRelations relations = do
  manyRelations <-
    Plan.execute
      (Plan.planMany describeRelationByName)
      relations

  let relationsMap =
        Map.mapMaybe id
          . Many.toMap
          $ manyRelations

  pure $
    DatabaseDescription
      { databaseRelations = relationsMap
      }

describeRelationByName :: Plan.Plan scope (NamespaceName, RelationName) (Maybe RelationDescription)
describeRelationByName =
  Plan.bind (fst <$> Plan.askParam) $ \namespaceName ->
    Plan.bind (snd <$> Plan.askParam) $ \relationName ->
      Plan.bind (Plan.using namespaceName findNamespace) $ \namespace ->
        let namespaceAndRelName =
              (,) <$> Plan.use namespace <*> Plan.use relationName
         in Plan.bind (Plan.chain namespaceAndRelName findRelation) $ \maybePgClass ->
              Plan.using maybePgClass (Plan.planMaybe describeRelationByClass)

describeRelationByClass :: Plan.Plan scope PgClass RelationDescription
describeRelationByClass =
  RelationDescription
    <$> Plan.askParam
    <*> findClassAttributes

findRelation :: Plan.Plan scope (PgNamespace, RelationName) (Maybe PgClass)
findRelation =
  Plan.focusParam (\(ns, relname) -> (pgNamespaceOid ns, relname)) $
    Plan.planOperation $
      Op.findOne pgClassTable byNamespaceOidAndRelationName

byNamespaceOidAndRelationName :: Op.WherePlanner (LibPQ.Oid, RelationName)
byNamespaceOidAndRelationName =
  Op.byFieldTuple namespaceOidField relationNameField

findNamespace :: Plan.Plan scope NamespaceName PgNamespace
findNamespace =
  Plan.findOne pgNamespaceTable namespaceNameField

findClassAttributes :: Plan.Plan scope PgClass (Map.Map AttributeName PgAttribute)
findClassAttributes =
  fmap (indexBy pgAttributeName) $
    Plan.focusParam pgClassOid $
      Plan.findAllWhere
        pgAttributeTable
        relationOidField
        (Orville.fieldEquals attributeIsDroppedField False)

indexBy :: Ord key => (row -> key) -> [row] -> Map.Map key row
indexBy rowKey =
  Map.fromList . fmap (\row -> (rowKey row, row))
