module Database.Orville.PostgreSQL.Internal.MigrationError
  ( MigrationError(..)
  ) where

import Control.Exception (Exception, SomeException, displayException)
import qualified Data.List as List
import Data.Typeable (Typeable)

import Database.Orville.PostgreSQL.Internal.Types

data MigrationError
  = MigrationLockExcessiveRetryError String
  | MigrationExecutionError SchemaItem
                            SomeException
  deriving (Typeable)

instance Exception MigrationError

--
-- This Show instance is not great, in fact it's horribly inconsistent. I'm leaving it like
-- this for now to avoid be derailed.
--
instance Show MigrationError where
  show err =
    case err of
      MigrationLockExcessiveRetryError msg ->
        "MigrationLockExcessiveRetryError " ++ show msg
      MigrationExecutionError (Table tableDef) someException ->
        formatTableMigrationException tableDef someException
      MigrationExecutionError schemaItem someException ->
        concat ["MigrationError ", show schemaItem, show someException]

formatTableMigrationException ::
     TableDefinition readEntity writeEntity key -> SomeException -> String
formatTableMigrationException tableDef exception = message
  where
    message =
      "There was an error migrating table " ++
      name ++
      ".\n\
                  \The error is:\n\
                  \\n\
                  \ " ++
      displayException exception ++
      "\\n\
                  \\n\
                  \\n\
                  \Here are the developer comments regarding the table:\n\
                  \\n\
                  \ " ++
      comments ++
      "\
                  \\n"
    name = tableName tableDef
    comments = formatTableComments " " tableDef

formatTableComments ::
     String -> TableDefinition readEntity writeEntity key -> String
formatTableComments indent tableDef =
  List.intercalate ("\n" ++ indent) commentLines
  where
    commentLines = map formatTableComment comments
    comments = runComments (tableComments tableDef)

formatTableComment :: TableComment -> String
formatTableComment c =
  List.intercalate " - " [tcWhat c, show (tcWhen c), tcWho c]
