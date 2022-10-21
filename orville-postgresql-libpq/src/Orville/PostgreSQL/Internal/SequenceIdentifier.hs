module Orville.PostgreSQL.Internal.SequenceIdentifier
  ( SequenceIdentifier,
    unqualifiedNameToSequenceId,
    setSequenceIdSchema,
    sequenceIdQualifiedName,
    sequenceIdUnqualifiedName,
    sequenceIdSchemaName,
    sequenceIdToString,
    sequenceIdUnqualifiedNameString,
    sequenceIdSchemaNameString,
  )
where

import qualified Orville.PostgreSQL.Internal.Expr as Expr

{- |
  An identifier used by Orville to identify a particular sequence in a particular
  schema.
-}
data SequenceIdentifier = SequenceIdentifier
  { _sequenceIdName :: String
  , _sequenceIdSchema :: Maybe String
  }
  deriving (Eq, Ord, Show)

{- |
  Constructs a 'SequenceIdentifier' where the sequence's name will not be qualified
  by a particular schema.
-}
unqualifiedNameToSequenceId :: String -> SequenceIdentifier
unqualifiedNameToSequenceId name =
  SequenceIdentifier
    { _sequenceIdName = name
    , _sequenceIdSchema = Nothing
    }

{- |
  Sets the schema of the 'SequenceIdentifier'. Wherever applicable, references to
  the sequence will be qualified by the given scheme name.
-}
setSequenceIdSchema :: String -> SequenceIdentifier -> SequenceIdentifier
setSequenceIdSchema schema sequenceId =
  sequenceId
    { _sequenceIdSchema = Just schema
    }

{- |
  Returns the 'Expr.Qualified Expr.SequenceName' that should be used to refer to the
  sequence in SQL queries.
-}
sequenceIdQualifiedName :: SequenceIdentifier -> Expr.Qualified Expr.SequenceName
sequenceIdQualifiedName sequenceId =
  Expr.qualified
    (sequenceIdSchemaName sequenceId)
    (sequenceIdUnqualifiedName sequenceId)

{- |
  Returns the unqualified 'Expr.SequenceName' that should be used to refer to the
  sequence in SQL queries where an unqualified reference is appropriate.
-}
sequenceIdUnqualifiedName :: SequenceIdentifier -> Expr.SequenceName
sequenceIdUnqualifiedName =
  Expr.sequenceName . _sequenceIdName

{- |
  Returns the 'Expr.SchemaName' (if any) that should be used to qualify
  references to the sequence in SQL queries.
-}
sequenceIdSchemaName :: SequenceIdentifier -> Maybe Expr.SchemaName
sequenceIdSchemaName =
  fmap Expr.schemaName . _sequenceIdSchema

{- |
  Retrieves the unqualified name of the sequence as a string.
-}
sequenceIdUnqualifiedNameString :: SequenceIdentifier -> String
sequenceIdUnqualifiedNameString =
  _sequenceIdName

{- |
  Retrieves the schema name of the sequence as a string
-}
sequenceIdSchemaNameString :: SequenceIdentifier -> Maybe String
sequenceIdSchemaNameString =
  _sequenceIdSchema

{- |
  Converts a 'SequenceIdentifier' for a string for descriptive purposes. The
  name will be qualified if a schema name has been set for the identifier.

  Note: You should not use this function for building SQL expressions. Use
  'sequenceIdQualifiedName' instead for that.
-}
sequenceIdToString :: SequenceIdentifier -> String
sequenceIdToString sequenceId =
  case _sequenceIdSchema sequenceId of
    Nothing ->
      _sequenceIdName sequenceId
    Just schema ->
      schema <> "." <> _sequenceIdName sequenceId
