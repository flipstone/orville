module Orville.PostgreSQL.PgCatalog.DatabaseDescription
  ( DatabaseDescription (..),
    RelationDescription (..),
    ConstraintDescription (..),
    ForeignRelationDescription (..),
    lookupRelation,
    lookupAttribute,
    describeDatabaseRelations,
  )
where

import Control.Applicative (liftA2)
import qualified Data.Map.Strict as Map
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField)
import Orville.PostgreSQL.PgCatalog.PgAttribute (AttributeName, AttributeNumber, PgAttribute (pgAttributeName, pgAttributeNumber), attributeIsDroppedField, attributeNumberToInt16, attributeRelationOidField, pgAttributeTable)
import Orville.PostgreSQL.PgCatalog.PgClass (PgClass (pgClassNamespaceOid, pgClassOid, pgClassRelationName), RelationName, namespaceOidField, pgClassTable, relationNameField, relationNameToString)
import Orville.PostgreSQL.PgCatalog.PgConstraint (PgConstraint (pgConstraintForeignKey, pgConstraintForeignRelationOid, pgConstraintKey), constraintRelationOidField, pgConstraintTable)
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
  , relationConstraints :: [ConstraintDescription]
  }

lookupAttribute ::
  AttributeName ->
  RelationDescription ->
  Maybe PgAttribute
lookupAttribute key =
  Map.lookup key . relationAttributes

{- |
  A description of a particular constraint in the PostgreSQL database, including
  the attributes and relations that it references.
-}
data ConstraintDescription = ConstraintDescription
  { constraintRecord :: PgConstraint
  , constraintKey :: Maybe [PgAttribute]
  , constraintForeignRelation :: Maybe ForeignRelationDescription
  , constraintForeignKey :: Maybe [PgAttribute]
  }

{- |
  A description of a relation in the PostgreSQL database that is referenced by
  a foreign key constraint, including the namespace that the relation belongs to.
-}
data ForeignRelationDescription = ForeignRelationDescription
  { foreignRelationClass :: PgClass
  , foreignRelationNamespace :: PgNamespace
  }

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
  Plan.bind Plan.askParam $ \pgClass ->
    Plan.bind findClassAttributes $ \attributes ->
      let classAndAttributes =
            (,)
              <$> Plan.use pgClass
              <*> Plan.use attributes
       in RelationDescription
            <$> Plan.use pgClass
            <*> Plan.use (fmap (indexBy pgAttributeName) attributes)
            <*> Plan.chain classAndAttributes findClassConstraints

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

findClassAttributes :: Plan.Plan scope PgClass [PgAttribute]
findClassAttributes =
  Plan.focusParam pgClassOid $
    Plan.findAllWhere
      pgAttributeTable
      attributeRelationOidField
      (Orville.fieldEquals attributeIsDroppedField False)

findClassConstraints :: Plan.Plan scope (PgClass, [PgAttribute]) [ConstraintDescription]
findClassConstraints =
  let relationOid :: (PgClass, [PgAttribute]) -> LibPQ.Oid
      relationOid (pgClass, _) =
        pgClassOid pgClass
   in Plan.bind (Plan.focusParam relationOid $ Plan.findAll pgConstraintTable constraintRelationOidField) $ \constraints ->
        Plan.bind (uncurry mkAttributeSource <$> Plan.askParam) $ \attrSource ->
          Plan.chain
            (zip <$> Plan.use (fmap repeat attrSource) <*> Plan.use constraints)
            (Plan.planList describeConstraint)

describeConstraint :: Plan.Plan scope (AttributeSource, PgConstraint) ConstraintDescription
describeConstraint =
  let prepareAttributeLookups :: (AttributeSource, PgConstraint) -> Maybe [(AttributeSource, AttributeNumber)]
      prepareAttributeLookups (attrSource, pgConstraint) =
        case pgConstraintKey pgConstraint of
          Nothing -> Nothing
          Just key -> Just (fmap (\attNum -> (attrSource, attNum)) key)
   in Plan.bind (snd <$> Plan.askParam) $ \constraint ->
        Plan.bind (Plan.using constraint findConstraintForeignRelationClass) $ \maybeForeignPgClass ->
          let maybeForeignClassAndAttrNums =
                liftA2
                  (liftA2 (,))
                  (Plan.use maybeForeignPgClass)
                  (fmap (pgConstraintForeignKey . snd) Plan.askParam)
           in ConstraintDescription
                <$> Plan.use constraint
                <*> Plan.focusParam prepareAttributeLookups (Plan.planMaybe $ Plan.planList findAttributeByNumber)
                <*> Plan.using maybeForeignPgClass (Plan.planMaybe describeForeignRelation)
                <*> Plan.chain maybeForeignClassAndAttrNums (Plan.planMaybe findForeignKeyAttributes)

describeForeignRelation :: Plan.Plan scope PgClass ForeignRelationDescription
describeForeignRelation =
  ForeignRelationDescription
    <$> Plan.askParam
    <*> Plan.focusParam pgClassNamespaceOid (Plan.findOne pgNamespaceTable oidField)

findForeignKeyAttributes :: Plan.Plan scope (PgClass, [AttributeNumber]) [PgAttribute]
findForeignKeyAttributes =
  Plan.bind (fst <$> Plan.askParam) $ \pgClass ->
    Plan.bind (snd <$> Plan.askParam) $ \attrNums ->
      Plan.bind (Plan.focusParam fst findClassAttributes) $ \attributes ->
        let attrSource =
              mkAttributeSource
                <$> Plan.use pgClass
                <*> Plan.use attributes
         in Plan.chain
              (zip <$> fmap repeat attrSource <*> Plan.use attrNums)
              (Plan.planList findAttributeByNumber)

data AttributeSource = AttributeSource
  { attrSourceName :: RelationName
  , attrSourceMap :: Map.Map AttributeNumber PgAttribute
  }

mkAttributeSource :: PgClass -> [PgAttribute] -> AttributeSource
mkAttributeSource pgClass attributes =
  AttributeSource
    { attrSourceName = pgClassRelationName pgClass
    , attrSourceMap = indexBy pgAttributeNumber attributes
    }

findAttributeByNumber :: Plan.Plan scope (AttributeSource, AttributeNumber) PgAttribute
findAttributeByNumber =
  let lookupAttr (attrSource, attrNum) =
        Map.lookup attrNum (attrSourceMap attrSource)

      assertFound ::
        (AttributeSource, AttributeNumber) ->
        Maybe PgAttribute ->
        Either String PgAttribute
      assertFound (attrSource, attrNum) maybeAttr =
        case maybeAttr of
          Nothing ->
            Left $
              "Unable to find attribute number "
                <> show (attributeNumberToInt16 attrNum)
                <> " of relation "
                <> relationNameToString (attrSourceName attrSource)
          Just attr ->
            Right attr
   in Plan.assert assertFound $ fmap lookupAttr Plan.askParam

findConstraintForeignRelationClass :: Plan.Plan scope PgConstraint (Maybe PgClass)
findConstraintForeignRelationClass =
  let relationId constraint =
        case pgConstraintForeignRelationOid constraint of
          LibPQ.Oid 0 -> Nothing
          nonZero -> Just nonZero
   in Plan.focusParam relationId $
        Plan.planMaybe $
          Plan.findOne pgClassTable oidField

indexBy :: Ord key => (row -> key) -> [row] -> Map.Map key row
indexBy rowKey =
  Map.fromList . fmap (\row -> (rowKey row, row))
