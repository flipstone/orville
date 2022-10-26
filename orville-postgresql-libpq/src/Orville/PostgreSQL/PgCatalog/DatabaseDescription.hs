module Orville.PostgreSQL.PgCatalog.DatabaseDescription
  ( DatabaseDescription (..),
    RelationDescription (..),
    ConstraintDescription (..),
    ForeignRelationDescription (..),
    IndexDescription (..),
    IndexMember (..),
    lookupRelation,
    lookupAttribute,
    lookupAttributeDefault,
    describeDatabaseRelations,
    describeRelationByClass,
  )
where

import Control.Applicative (liftA2)
import qualified Data.Map.Strict as Map
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField)
import Orville.PostgreSQL.PgCatalog.PgAttribute (AttributeName, AttributeNumber, PgAttribute (pgAttributeName, pgAttributeNumber), attributeIsDroppedField, attributeNumberToInt16, attributeRelationOidField, pgAttributeTable)
import Orville.PostgreSQL.PgCatalog.PgAttributeDefault (PgAttributeDefault (pgAttributeDefaultAttributeNumber), attributeDefaultRelationOidField, pgAttributeDefaultTable)
import Orville.PostgreSQL.PgCatalog.PgClass (PgClass (pgClassNamespaceOid, pgClassOid, pgClassRelationName), RelationName, namespaceOidField, pgClassTable, relationNameField, relationNameToString)
import Orville.PostgreSQL.PgCatalog.PgConstraint (PgConstraint (pgConstraintForeignKey, pgConstraintForeignRelationOid, pgConstraintKey), constraintRelationOidField, pgConstraintTable)
import Orville.PostgreSQL.PgCatalog.PgIndex (PgIndex (pgIndexAttributeNumbers, pgIndexPgClassOid), indexIsLiveField, indexRelationOidField, pgIndexTable)
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
  , relationAttributeDefaults :: Map.Map AttributeNumber PgAttributeDefault
  , relationConstraints :: [ConstraintDescription]
  , relationIndexes :: [IndexDescription]
  }

{- |
  Find an attribute by name from the 'RelationDescription'
-}
lookupAttribute ::
  AttributeName ->
  RelationDescription ->
  Maybe PgAttribute
lookupAttribute key =
  Map.lookup key . relationAttributes

{- |
  Find an attribute default from the 'RelationDescription'
-}
lookupAttributeDefault ::
  PgAttribute ->
  RelationDescription ->
  Maybe PgAttributeDefault
lookupAttributeDefault attr =
  Map.lookup (pgAttributeNumber attr) . relationAttributeDefaults

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
  A description of an index in the PostgreSQL database, including the names of
  the attributes included in the index and the 'PgClass' record of the index
  itself (NOT the 'PgClass' of the table that the index is for).
-}
data IndexDescription = IndexDescription
  { indexRecord :: PgIndex
  , indexPgClass :: PgClass
  , indexMembers :: [IndexMember]
  }

{- |
  A description of an index member in the PostgreSQL database. If they member
  is a simple attribute, the 'PgAttribute' for that is provided. If it is an
  index over an expression, no further description is currently provided.
-}
data IndexMember
  = IndexAttribute PgAttribute
  | IndexExpression

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
            mkPgClassAndAttributes
              <$> Plan.use pgClass
              <*> Plan.use attributes
       in RelationDescription
            <$> Plan.use pgClass
            <*> Plan.use (fmap (indexBy pgAttributeName) attributes)
            <*> fmap (indexBy pgAttributeDefaultAttributeNumber) findClassAttributeDefaults
            <*> Plan.chain classAndAttributes findClassConstraints
            <*> Plan.chain classAndAttributes findClassIndexes

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

findClassAttributeDefaults :: Plan.Plan scope PgClass [PgAttributeDefault]
findClassAttributeDefaults =
  Plan.focusParam pgClassOid $
    Plan.findAll
      pgAttributeDefaultTable
      attributeDefaultRelationOidField

findClassConstraints :: Plan.Plan scope PgClassAndAttributes [ConstraintDescription]
findClassConstraints =
  let relationOid =
        pgClassOid . pgClassRecord
   in Plan.bind (Plan.focusParam relationOid $ Plan.findAll pgConstraintTable constraintRelationOidField) $ \constraints ->
        Plan.bind Plan.askParam $ \pgClassAndAttrs ->
          Plan.chain
            (zip <$> Plan.use (fmap repeat pgClassAndAttrs) <*> Plan.use constraints)
            (Plan.planList describeConstraint)

describeConstraint :: Plan.Plan scope (PgClassAndAttributes, PgConstraint) ConstraintDescription
describeConstraint =
  let prepareAttributeLookups :: (PgClassAndAttributes, PgConstraint) -> Maybe [(PgClassAndAttributes, AttributeNumber)]
      prepareAttributeLookups (pgClassAndAttrs, pgConstraint) =
        case pgConstraintKey pgConstraint of
          Nothing -> Nothing
          Just key -> Just (fmap (\attNum -> (pgClassAndAttrs, attNum)) key)
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
              mkPgClassAndAttributes
                <$> Plan.use pgClass
                <*> Plan.use attributes
         in Plan.chain
              (zip <$> fmap repeat attrSource <*> Plan.use attrNums)
              (Plan.planList findAttributeByNumber)

findConstraintForeignRelationClass :: Plan.Plan scope PgConstraint (Maybe PgClass)
findConstraintForeignRelationClass =
  let relationId constraint =
        case pgConstraintForeignRelationOid constraint of
          LibPQ.Oid 0 -> Nothing
          nonZero -> Just nonZero
   in Plan.focusParam relationId $
        Plan.planMaybe $
          Plan.findOne pgClassTable oidField

findClassIndexes :: Plan.Plan scope PgClassAndAttributes [IndexDescription]
findClassIndexes =
  let findIndexes :: Plan.Plan scope PgClassAndAttributes [PgIndex]
      findIndexes =
        Plan.focusParam (pgClassOid . pgClassRecord) $
          Plan.findAllWhere
            pgIndexTable
            indexRelationOidField
            (Orville.fieldEquals indexIsLiveField True)

      indexesWithClassAndAttrs :: Plan.Plan scope PgClassAndAttributes [(PgClassAndAttributes, PgIndex)]
      indexesWithClassAndAttrs =
        zip
          <$> fmap repeat Plan.askParam
          <*> findIndexes
   in Plan.chain indexesWithClassAndAttrs (Plan.planList describeIndex)

describeIndex :: Plan.Plan scope (PgClassAndAttributes, PgIndex) IndexDescription
describeIndex =
  let expressionsOrAttributeLookups ::
        PgClassAndAttributes ->
        [AttributeNumber] ->
        [Either IndexMember (PgClassAndAttributes, AttributeNumber)]
      expressionsOrAttributeLookups pgClassAndAttrs attNumList = do
        attNum <- attNumList
        pure $
          if attNum == 0
            then Left IndexExpression
            else Right (pgClassAndAttrs, attNum)

      indexMemberLookups ::
        Plan.Plan
          scope
          (PgClassAndAttributes, PgIndex)
          [Either IndexMember (PgClassAndAttributes, AttributeNumber)]
      indexMemberLookups =
        Plan.bind (fst <$> Plan.askParam) $ \pgClassAndAttrs ->
          Plan.bind (pgIndexAttributeNumbers . snd <$> Plan.askParam) $ \attNums ->
            expressionsOrAttributeLookups
              <$> Plan.use pgClassAndAttrs
              <*> Plan.use attNums

      resolveIndexMemberLookup ::
        Plan.Plan
          scope
          (Either IndexMember (PgClassAndAttributes, AttributeNumber))
          IndexMember
      resolveIndexMemberLookup =
        either id IndexAttribute
          <$> Plan.planEither Plan.askParam findAttributeByNumber
   in IndexDescription
        <$> fmap snd Plan.askParam
        <*> Plan.focusParam (pgIndexPgClassOid . snd) (Plan.findOne pgClassTable oidField)
        <*> Plan.chain indexMemberLookups (Plan.planList resolveIndexMemberLookup)

data PgClassAndAttributes = PgClassAndAttributes
  { pgClassRecord :: PgClass
  , pgClassAttributes :: Map.Map AttributeNumber PgAttribute
  }

mkPgClassAndAttributes :: PgClass -> [PgAttribute] -> PgClassAndAttributes
mkPgClassAndAttributes pgClass attributes =
  PgClassAndAttributes
    { pgClassRecord = pgClass
    , pgClassAttributes = indexBy pgAttributeNumber attributes
    }

findAttributeByNumber :: Plan.Plan scope (PgClassAndAttributes, AttributeNumber) PgAttribute
findAttributeByNumber =
  let lookupAttr (pgClassAndAttrs, attrNum) =
        Map.lookup attrNum (pgClassAttributes pgClassAndAttrs)

      assertFound ::
        (PgClassAndAttributes, AttributeNumber) ->
        Maybe PgAttribute ->
        Either String PgAttribute
      assertFound (pgClassAndAttrs, attrNum) maybeAttr =
        case maybeAttr of
          Nothing ->
            Left $
              "Unable to find attribute number "
                <> show (attributeNumberToInt16 attrNum)
                <> " of relation "
                <> (relationNameToString . pgClassRelationName . pgClassRecord $ pgClassAndAttrs)
          Just attr ->
            Right attr
   in Plan.assert assertFound $ fmap lookupAttr Plan.askParam

indexBy :: Ord key => (row -> key) -> [row] -> Map.Map key row
indexBy rowKey =
  Map.fromList . fmap (\row -> (rowKey row, row))
