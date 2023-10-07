{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Schema.IndexDefinition
  ( IndexDefinition.IndexDefinition
  , IndexDefinition.indexCreationStrategy
  , IndexDefinition.setIndexCreationStrategy
  , IndexDefinition.uniqueIndex
  , IndexDefinition.uniqueNamedIndex
  , IndexDefinition.nonUniqueIndex
  , IndexDefinition.nonUniqueNamedIndex
  , IndexDefinition.mkIndexDefinition
  , IndexDefinition.mkNamedIndexDefinition
  , Expr.IndexUniqueness (UniqueIndex, NonUniqueIndex)
  , IndexDefinition.indexCreateExpr
  , IndexDefinition.IndexCreationStrategy (Transactional, Asynchronous)
  )
where

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.IndexDefinition as IndexDefinition
