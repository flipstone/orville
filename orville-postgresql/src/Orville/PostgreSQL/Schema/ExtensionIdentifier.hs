{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Schema.ExtensionIdentifier
  ( ExtensionIdentifier
  , nameToExtensionId
  , extensionIdName
  , extensionIdToString
  )
where

import qualified Orville.PostgreSQL.Expr as Expr

{- |
  An identifier used by Orville to identify a particular extension

@since 1.1.0.0
-}
data ExtensionIdentifier = ExtensionIdentifier
  { i_extensionName :: String
  }
  deriving (Eq, Ord, Show)

{- |
  Constructs a 'ExtensionIdentifier' with the given extension name string.

@since 1.1.0.0
-}
nameToExtensionId :: String -> ExtensionIdentifier
nameToExtensionId name =
  ExtensionIdentifier
    { i_extensionName = name
    }

{- |
  Returns the 'Expr.ExtensionName' that should be used to refer to the extension in SQL queries.

@since 1.1.0.0
-}
extensionIdName :: ExtensionIdentifier -> Expr.ExtensionName
extensionIdName =
  Expr.extensionName . extensionIdToString

{- |
  Converts a 'ExtensionIdentifier' to a 'String' for descriptive purposes. The
  name will be qualified if a schema name has been set for the identifier.

  Note: You should not use this function for building SQL expressions. Use
  'extensionIdName' instead for that.

@since 1.1.0.0
-}
extensionIdToString :: ExtensionIdentifier -> String
extensionIdToString = i_extensionName
