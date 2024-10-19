{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Schema.SequenceIdentifier
  ( SequenceIdentifier
  , unqualifiedNameToSequenceId
  , setSequenceIdSchema
  , sequenceIdQualifiedName
  , sequenceIdUnqualifiedName
  , sequenceIdSchemaName
  , sequenceIdToString
  , sequenceIdUnqualifiedNameString
  , sequenceIdSchemaNameString
  )
where

import qualified Orville.PostgreSQL.Expr as Expr

{- |
  An identifier used by Orville to identify a particular sequence in a particular
  schema.

@since 1.0.0.0
-}
data SequenceIdentifier = SequenceIdentifier
  { i_sequenceIdName :: String
  , i_sequenceIdSchema :: Maybe String
  }
  deriving
    ( -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Ord
    , -- | @since 1.0.0.0
      Show
    )

{- |
  Constructs a 'SequenceIdentifier' where the sequence's name will not be qualified
  by a particular schema.

@since 1.0.0.0
-}
unqualifiedNameToSequenceId :: String -> SequenceIdentifier
unqualifiedNameToSequenceId name =
  SequenceIdentifier
    { i_sequenceIdName = name
    , i_sequenceIdSchema = Nothing
    }

{- |
  Sets the schema of the 'SequenceIdentifier'. Wherever applicable, references
  to the sequence will be qualified by the given schema name.

@since 1.0.0.0
-}
setSequenceIdSchema :: String -> SequenceIdentifier -> SequenceIdentifier
setSequenceIdSchema schema sequenceId =
  sequenceId
    { i_sequenceIdSchema = Just schema
    }

{- |
  Returns the 'Expr.Qualified Expr.SequenceName' that should be used to refer to the
  sequence in SQL queries.

@since 1.0.0.0
-}
sequenceIdQualifiedName :: SequenceIdentifier -> Expr.Qualified Expr.SequenceName
sequenceIdQualifiedName sequenceId =
  Expr.qualifySequence
    (sequenceIdSchemaName sequenceId)
    (sequenceIdUnqualifiedName sequenceId)

{- |
  Returns the unqualified 'Expr.SequenceName' that should be used to refer to the
  sequence in SQL queries where an unqualified reference is appropriate.

@since 1.0.0.0
-}
sequenceIdUnqualifiedName :: SequenceIdentifier -> Expr.SequenceName
sequenceIdUnqualifiedName =
  Expr.sequenceName . i_sequenceIdName

{- |
  Returns the 'Expr.SchemaName' (if any) that should be used to qualify
  references to the sequence in SQL queries.

@since 1.0.0.0
-}
sequenceIdSchemaName :: SequenceIdentifier -> Maybe Expr.SchemaName
sequenceIdSchemaName =
  fmap Expr.schemaName . i_sequenceIdSchema

{- |
  Retrieves the unqualified name of the sequence as a 'String'.

@since 1.0.0.0
-}
sequenceIdUnqualifiedNameString :: SequenceIdentifier -> String
sequenceIdUnqualifiedNameString =
  i_sequenceIdName

{- |
  Retrieves the schema name of the sequence as a 'String'.

@since 1.0.0.0
-}
sequenceIdSchemaNameString :: SequenceIdentifier -> Maybe String
sequenceIdSchemaNameString =
  i_sequenceIdSchema

{- |
  Converts a 'SequenceIdentifier' for a 'String' for descriptive purposes. The
  name will be qualified if a schema name has been set for the identifier.

  Note: You should not use this function for building SQL expressions. Use
  'sequenceIdQualifiedName' instead for that.

@since 1.0.0.0
-}
sequenceIdToString :: SequenceIdentifier -> String
sequenceIdToString sequenceId =
  case i_sequenceIdSchema sequenceId of
    Nothing ->
      i_sequenceIdName sequenceId
    Just schema ->
      schema <> "." <> i_sequenceIdName sequenceId
