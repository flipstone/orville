{-# LANGUAGE CPP #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.PgCatalog.DatabaseDescription
  ( DatabaseDescription (..)
  , RelationDescription (..)
  , ConstraintDescription (..)
  , ForeignRelationDescription (..)
  , IndexDescription (..)
  , IndexMember (..)
  , lookupRelation
  , lookupRelationOfKind
  , lookupAttribute
  , lookupAttributeDefault
  , lookupAttributeComment
  , lookupProcedure
  , lookupExtension
  , describeDatabase
  )
where

#if MIN_VERSION_base(4,18,0)
#else
import Control.Applicative (liftA2)
#endif
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField)
import Orville.PostgreSQL.PgCatalog.PgAttribute (AttributeName, AttributeNumber, PgAttribute (pgAttributeName, pgAttributeNumber), attributeIsDroppedField, attributeNumberToInt16, attributeRelationOidField, pgAttributeTable)
import Orville.PostgreSQL.PgCatalog.PgAttributeDefault (PgAttributeDefault (pgAttributeDefaultAttributeNumber), attributeDefaultRelationOidField, pgAttributeDefaultTable)
import Orville.PostgreSQL.PgCatalog.PgClass (PgClass (pgClassNamespaceOid, pgClassOid, pgClassRelationName), RelationKind, RelationName, namespaceOidField, pgClassRelationKind, pgClassTable, relationNameField, relationNameToString)
import Orville.PostgreSQL.PgCatalog.PgConstraint (PgConstraint (pgConstraintForeignKey, pgConstraintForeignRelationOid, pgConstraintKey), constraintRelationOidField, pgConstraintTable)
import Orville.PostgreSQL.PgCatalog.PgDescription (ObjectSubId, PgDescription (pgDescriptionObjSubId), objOidField, objectSubIdFromAttributeNumber, objectSubIdZero, pgDescriptionDescription, pgDescriptionTable)
import Orville.PostgreSQL.PgCatalog.PgExtension (ExtensionName, PgExtension, extensionNameField, pgExtensionTable)
import Orville.PostgreSQL.PgCatalog.PgIndex (PgIndex (pgIndexAttributeNumbers, pgIndexPgClassOid), indexIsLiveField, indexRelationOidField, pgIndexTable)
import Orville.PostgreSQL.PgCatalog.PgNamespace (NamespaceName, PgNamespace (pgNamespaceOid), namespaceNameField, pgNamespaceTable)
import Orville.PostgreSQL.PgCatalog.PgProc (PgProc, ProcName, pgProcTable, procNameField, procNamespaceOidField)
import Orville.PostgreSQL.PgCatalog.PgSequence (PgSequence, pgSequenceTable, sequencePgClassOidField)
import Orville.PostgreSQL.PgCatalog.PgTrigger (PgTrigger, pgTriggerTable, triggerRelationOidField)
import qualified Orville.PostgreSQL.Plan as Plan
import qualified Orville.PostgreSQL.Plan.Many as Many
import qualified Orville.PostgreSQL.Plan.Operation as Op

{- | A description of selected items from a single PostgreSQL database.
  'describeDatabase' can be used to load the descriptions of request
  items.

@since 1.0.0.0
-}
data DatabaseDescription = DatabaseDescription
  { databaseRelations :: Map.Map (NamespaceName, RelationName) RelationDescription
  -- ^ @since 1.0.0.0
  , databaseProcedures :: Map.Map (NamespaceName, ProcName) PgProc
  -- ^ @since 1.1.0.0
  , databaseExtensions :: Map.Map ExtensionName PgExtension
  -- ^ @since 1.1.0.0
  }

{- | Lookup a relation by its qualified name in the @pg_catalog@ schema.

@since 1.0.0.0
-}
lookupRelation ::
  (NamespaceName, RelationName) ->
  DatabaseDescription ->
  Maybe RelationDescription
lookupRelation key =
  Map.lookup key . databaseRelations

{- | Lookup a procedure by its qualified name in the @pg_catalog@ schema.

@since 1.1.0.0
-}
lookupProcedure ::
  (NamespaceName, ProcName) ->
  DatabaseDescription ->
  Maybe PgProc
lookupProcedure key =
  Map.lookup key . databaseProcedures

{- | Lookup an extension by its name in the @pg_catalog@ schema.

@since 1.1.0.0
-}
lookupExtension ::
  ExtensionName ->
  DatabaseDescription ->
  Maybe PgExtension
lookupExtension key =
  Map.lookup key . databaseExtensions

{- | Lookup a relation by its qualified name in the @pg_catalog@ schema. If the
  relation is not of the expected kind, 'Nothing' is returned.

@since 1.0.0.0
-}
lookupRelationOfKind ::
  RelationKind ->
  (NamespaceName, RelationName) ->
  DatabaseDescription ->
  Maybe RelationDescription
lookupRelationOfKind kind key dbDesc =
  case Map.lookup key (databaseRelations dbDesc) of
    Just relation ->
      if pgClassRelationKind (relationRecord relation) == kind
        then Just relation
        else Nothing
    Nothing ->
      Nothing

{- | A description of a particular relation in the PostgreSQL database, including
  the attributes of the relation.

@since 1.0.0.0
-}
data RelationDescription = RelationDescription
  { relationRecord :: PgClass
  -- ^ @since 1.0.0.0
  , relationAttributes :: Map.Map AttributeName PgAttribute
  -- ^ @since 1.0.0.0
  , relationAttributeDefaults :: Map.Map AttributeNumber PgAttributeDefault
  -- ^ @since 1.0.0.0
  , relationConstraints :: [ConstraintDescription]
  -- ^ @since 1.0.0.0
  , relationIndexes :: [IndexDescription]
  -- ^ @since 1.0.0.0
  , relationTriggers :: [PgTrigger]
  -- ^ @since 1.1.0.0
  , relationSequence :: Maybe PgSequence
  -- ^ @since 1.0.0.0
  , relationComment :: Maybe T.Text
  -- ^ @since 1.1.0.0
  , relationAttributeComments :: Map.Map AttributeName T.Text
  -- ^ @since 1.1.0.0
  }

{- | Find an attribute by name from the 'RelationDescription'.

@since 1.0.0.0
-}
lookupAttribute ::
  AttributeName ->
  RelationDescription ->
  Maybe PgAttribute
lookupAttribute key =
  Map.lookup key . relationAttributes

{- | Find an attribute default from the 'RelationDescription'.

@since 1.0.0.0
-}
lookupAttributeDefault ::
  PgAttribute ->
  RelationDescription ->
  Maybe PgAttributeDefault
lookupAttributeDefault attr =
  Map.lookup (pgAttributeNumber attr) . relationAttributeDefaults

{- | Find an attribute comment by name from the 'RelationDescription'

@since 1.1.0.0
-}
lookupAttributeComment ::
  AttributeName ->
  RelationDescription ->
  Maybe T.Text
lookupAttributeComment key =
  Map.lookup key . relationAttributeComments

{- | A description of a particular constraint in the PostgreSQL database, including
  the attributes and relations that it references.

@since 1.0.0.0
-}
data ConstraintDescription = ConstraintDescription
  { constraintRecord :: PgConstraint
  , constraintKey :: Maybe [PgAttribute]
  , constraintForeignRelation :: Maybe ForeignRelationDescription
  , constraintForeignKey :: Maybe [PgAttribute]
  }

{- | A description of a relation in the PostgreSQL database that is referenced by
  a foreign key constraint, including the namespace that the relation belongs to.

@since 1.0.0.0
-}
data ForeignRelationDescription = ForeignRelationDescription
  { foreignRelationClass :: PgClass
  , foreignRelationNamespace :: PgNamespace
  }

{- | A description of an index in the PostgreSQL database, including the names of
  the attributes included in the index and the 'PgClass' record of the index
  itself (NOT the 'PgClass' of the table that the index is for).

@since 1.0.0.0
-}
data IndexDescription = IndexDescription
  { indexRecord :: PgIndex
  , indexPgClass :: PgClass
  , indexMembers :: [IndexMember]
  }

{- | A description of an index member in the PostgreSQL database. If the member
  is a simple attribute, the 'PgAttribute' for that is provided. If it is an
  index over an expression, no further description is currently provided.

@since 1.0.0.0
-}
data IndexMember
  = IndexAttribute PgAttribute
  | IndexExpression

{- | Describes the requested relations in the current database. If any of the
  relations do not exist, they will not have an entry in the returned
  description.

  Each 'RelationDescription' will contain all the attributes that currently
  exist for that relation, according to the @pg_catalog@ tables.

@since 1.1.0.0
-}
describeDatabase ::
  Orville.MonadOrville m =>
  [(NamespaceName, RelationName)] ->
  [(NamespaceName, ProcName)] ->
  [ExtensionName] ->
  m DatabaseDescription
describeDatabase relations procedures extensions = do
  manyRelations <-
    Plan.execute
      (Plan.planMany describeRelationByName)
      relations

  manyProcedures <-
    Plan.execute
      (Plan.planMany describeProcedureByName)
      procedures

  manyExtensions <-
    Plan.execute
      (Plan.planMany describeExtensionByName)
      extensions

  let
    mkMap :: Ord k => Many.Many k (Maybe v) -> Map.Map k v
    mkMap =
      Map.mapMaybe id . Many.toMap

  pure $
    DatabaseDescription
      { databaseRelations = mkMap manyRelations
      , databaseProcedures = mkMap manyProcedures
      , databaseExtensions = mkMap manyExtensions
      }

describeRelationByName :: Plan.Plan scope (NamespaceName, RelationName) (Maybe RelationDescription)
describeRelationByName =
  Plan.bind (fst <$> Plan.askParam) $ \namespaceName ->
    Plan.bind (snd <$> Plan.askParam) $ \relationName ->
      Plan.bind (Plan.using namespaceName findNamespace) $ \namespace ->
        let
          namespaceAndRelName =
            (,) <$> Plan.use namespace <*> Plan.use relationName
        in
          Plan.bind (Plan.chain namespaceAndRelName findRelation) $ \maybePgClass ->
            Plan.using maybePgClass (Plan.planMaybe describeRelationByClass)

describeRelationByClass :: Plan.Plan scope PgClass RelationDescription
describeRelationByClass =
  Plan.bind Plan.askParam $ \pgClass ->
    Plan.bind findClassAttributes $ \attributes ->
      Plan.bind findClassDescriptions $ \descriptions ->
        let
          classAndAttributes =
            mkPgClassAndAttributes
              <$> Plan.use pgClass
              <*> Plan.use attributes
        in
          RelationDescription
            <$> Plan.use pgClass
            <*> Plan.use (fmap (indexBy pgAttributeName) attributes)
            <*> fmap (indexBy pgAttributeDefaultAttributeNumber) findClassAttributeDefaults
            <*> Plan.chain classAndAttributes findClassConstraints
            <*> Plan.chain classAndAttributes findClassIndexes
            <*> Plan.using pgClass findClassTriggers
            <*> Plan.using pgClass findClassSequence
            <*> fmap (Map.lookup objectSubIdZero) (Plan.use descriptions)
            <*> liftA2 mkAttributeCommentMap (Plan.use descriptions) (Plan.use attributes)

findRelation :: Plan.Plan scope (PgNamespace, RelationName) (Maybe PgClass)
findRelation =
  Plan.focusParam (\(ns, relname) -> (pgNamespaceOid ns, relname))
    . Plan.planOperation
    $ Op.findOne pgClassTable byNamespaceOidAndRelationName

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
  let
    relationOid =
      pgClassOid . pgClassRecord
  in
    Plan.bind (Plan.focusParam relationOid $ Plan.findAll pgConstraintTable constraintRelationOidField) $ \constraints ->
      Plan.bind Plan.askParam $ \pgClassAndAttrs ->
        Plan.chain
          (zip <$> Plan.use (fmap repeat pgClassAndAttrs) <*> Plan.use constraints)
          (Plan.planList describeConstraint)

describeConstraint :: Plan.Plan scope (PgClassAndAttributes, PgConstraint) ConstraintDescription
describeConstraint =
  let
    prepareAttributeLookups :: (PgClassAndAttributes, PgConstraint) -> Maybe [(PgClassAndAttributes, AttributeNumber)]
    prepareAttributeLookups (pgClassAndAttrs, pgConstraint) =
      case pgConstraintKey pgConstraint of
        Nothing -> Nothing
        Just key -> Just (fmap (\attNum -> (pgClassAndAttrs, attNum)) key)
  in
    Plan.bind (snd <$> Plan.askParam) $ \constraint ->
      Plan.bind (Plan.using constraint findConstraintForeignRelationClass) $ \maybeForeignPgClass ->
        let
          maybeForeignClassAndAttrNums =
            liftA2
              (liftA2 (,))
              (Plan.use maybeForeignPgClass)
              (fmap (pgConstraintForeignKey . snd) Plan.askParam)
        in
          ConstraintDescription
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
        let
          attrSource =
            mkPgClassAndAttributes
              <$> Plan.use pgClass
              <*> Plan.use attributes
        in
          Plan.chain
            (zip <$> fmap repeat attrSource <*> Plan.use attrNums)
            (Plan.planList findAttributeByNumber)

findConstraintForeignRelationClass :: Plan.Plan scope PgConstraint (Maybe PgClass)
findConstraintForeignRelationClass =
  let
    relationId constraint =
      case pgConstraintForeignRelationOid constraint of
        LibPQ.Oid 0 -> Nothing
        nonZero -> Just nonZero
  in
    Plan.focusParam relationId
      . Plan.planMaybe
      $ Plan.findOne pgClassTable oidField

findClassIndexes :: Plan.Plan scope PgClassAndAttributes [IndexDescription]
findClassIndexes =
  let
    findIndexes :: Plan.Plan scope PgClassAndAttributes [PgIndex]
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
  in
    Plan.chain indexesWithClassAndAttrs (Plan.planList describeIndex)

describeIndex :: Plan.Plan scope (PgClassAndAttributes, PgIndex) IndexDescription
describeIndex =
  let
    expressionsOrAttributeLookups ::
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
  in
    IndexDescription
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
  let
    lookupAttr (pgClassAndAttrs, attrNum) =
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
  in
    Plan.assert assertFound $ fmap lookupAttr Plan.askParam

findClassSequence :: Plan.Plan scope PgClass (Maybe PgSequence)
findClassSequence =
  Plan.focusParam pgClassOid $
    Plan.findMaybeOne pgSequenceTable sequencePgClassOidField

findClassDescriptions :: Plan.Plan scope PgClass (Map.Map ObjectSubId T.Text)
findClassDescriptions =
  let
    mkObjectSubIdMap =
      Map.fromList
        . fmap
          ( \pgDescription ->
              (pgDescriptionObjSubId pgDescription, pgDescriptionDescription pgDescription)
          )
  in
    fmap mkObjectSubIdMap . Plan.focusParam pgClassOid $ Plan.findAll pgDescriptionTable objOidField

mkAttributeCommentMap :: Map.Map ObjectSubId T.Text -> [PgAttribute] -> Map.Map AttributeName T.Text
mkAttributeCommentMap descriptionMap =
  foldMap
    ( \attribute ->
        maybe
          mempty
          (Map.singleton (pgAttributeName attribute))
          ( Map.lookup
              (objectSubIdFromAttributeNumber $ pgAttributeNumber attribute)
              descriptionMap
          )
    )

findClassTriggers :: Plan.Plan scope PgClass [PgTrigger]
findClassTriggers =
  Plan.focusParam pgClassOid $
    Plan.findAll pgTriggerTable triggerRelationOidField

describeProcedureByName :: Plan.Plan scope (NamespaceName, ProcName) (Maybe PgProc)
describeProcedureByName =
  Plan.bind (fst <$> Plan.askParam) $ \namespaceName ->
    Plan.bind (snd <$> Plan.askParam) $ \procName ->
      Plan.bind (Plan.using namespaceName findNamespace) $ \namespace ->
        let
          namespaceAndProcName =
            (,) <$> Plan.use namespace <*> Plan.use procName
        in
          Plan.chain namespaceAndProcName findProc

findProc :: Plan.Plan scope (PgNamespace, ProcName) (Maybe PgProc)
findProc =
  Plan.focusParam (\(ns, procName) -> (pgNamespaceOid ns, procName))
    . Plan.planOperation
    $ Op.findOne pgProcTable byNamespaceOidAndProcName

byNamespaceOidAndProcName :: Op.WherePlanner (LibPQ.Oid, ProcName)
byNamespaceOidAndProcName =
  Op.byFieldTuple procNamespaceOidField procNameField

describeExtensionByName :: Plan.Plan scope ExtensionName (Maybe PgExtension)
describeExtensionByName =
  Plan.findMaybeOne pgExtensionTable extensionNameField

indexBy :: Ord key => (row -> key) -> [row] -> Map.Map key row
indexBy rowKey =
  Map.fromList . fmap (\row -> (rowKey row, row))
