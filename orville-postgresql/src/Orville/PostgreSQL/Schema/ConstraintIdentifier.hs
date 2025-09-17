{- |
Copyright : Flipstone Technology Partners 2025
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Schema.ConstraintIdentifier
  ( ConstraintIdentifier
  , unqualifiedNameToConstraintId
  , setConstraintIdSchema
  , constraintIdQualifiedName
  , constraintIdUnqualifiedName
  , constraintIdSchemaName
  , constraintIdToString
  , constraintIdUnqualifiedNameString
  , constraintIdSchemaNameString
  )
where

import qualified Orville.PostgreSQL.Expr as Expr

{- | An identifier used by Orville to identify a particular constraint in a particular
  schema.

@since 1.2.0.0
-}
data ConstraintIdentifier = ConstraintIdentifier
  { i_constraintIdName :: String
  , i_constraintIdSchema :: Maybe String
  }
  deriving
    ( -- | @since 1.2.0.0
      Eq
    , -- | @since 1.2.0.0
      Ord
    , -- | @since 1.2.0.0
      Show
    )

{- | Constructs a 'ConstraintIdentifier' where the constraint's name will not be qualified
  by a particular schema.

@since 1.2.0.0
-}
unqualifiedNameToConstraintId :: String -> ConstraintIdentifier
unqualifiedNameToConstraintId name =
  ConstraintIdentifier
    { i_constraintIdName = name
    , i_constraintIdSchema = Nothing
    }

{- | Sets the schema of the 'ConstraintIdentifier'. Wherever applicable, references to
  the constraint will be qualified by the given schema name.

@since 1.2.0.0
-}
setConstraintIdSchema :: String -> ConstraintIdentifier -> ConstraintIdentifier
setConstraintIdSchema schema constraintId =
  constraintId
    { i_constraintIdSchema = Just schema
    }

{- | Returns the 'Expr.Qualified Expr.ConstraintName' that should be used to refer to
  the constraint in SQL queries.

@since 1.2.0.0
-}
constraintIdQualifiedName :: ConstraintIdentifier -> Expr.QualifiedOrUnqualified Expr.ConstraintName
constraintIdQualifiedName constraintId =
  case constraintIdSchemaName constraintId of
    Nothing ->
      Expr.unqualified (constraintIdUnqualifiedName constraintId)
    Just schemaName ->
      Expr.untrackQualified $
        Expr.qualifyConstraint
          schemaName
          (constraintIdUnqualifiedName constraintId)

{- | Returns the unqualified 'Expr.ConstraintName' that should be used to refer to the
  constraint in SQL queries where an unqualified reference is appropriate.

@since 1.2.0.0
-}
constraintIdUnqualifiedName :: ConstraintIdentifier -> Expr.ConstraintName
constraintIdUnqualifiedName =
  Expr.constraintName . i_constraintIdName

{- | Returns the 'Expr.SchemaName' (if any) that should be used to qualify
  references to the constraint in SQL queries.

@since 1.2.0.0
-}
constraintIdSchemaName :: ConstraintIdentifier -> Maybe Expr.SchemaName
constraintIdSchemaName =
  fmap Expr.schemaName . i_constraintIdSchema

{- | Retrieves the unqualified name of the constraint as a 'String'.

@since 1.2.0.0
-}
constraintIdUnqualifiedNameString :: ConstraintIdentifier -> String
constraintIdUnqualifiedNameString =
  i_constraintIdName

{- | Retrieves the schema name of the constraint as a 'String'.

@since 1.2.0.0
-}
constraintIdSchemaNameString :: ConstraintIdentifier -> Maybe String
constraintIdSchemaNameString =
  i_constraintIdSchema

{- | Converts a 'ConstraintIdentifier' to a 'String' for descriptive purposes. The
  name will be qualified if a schema name has been set for the identifier.

  Note: You should not use this function for building SQL expressions. Use
  'constraintIdQualifiedName' instead for that.

@since 1.2.0.0
-}
constraintIdToString :: ConstraintIdentifier -> String
constraintIdToString constraintId =
  case i_constraintIdSchema constraintId of
    Nothing ->
      i_constraintIdName constraintId
    Just schema ->
      schema <> "." <> i_constraintIdName constraintId
