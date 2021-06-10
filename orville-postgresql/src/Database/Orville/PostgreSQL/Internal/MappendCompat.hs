{-|
Module    : Database.Orville.PostgreSQL.Internal.Expr.NameExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE CPP #-}

module Database.Orville.PostgreSQL.Internal.MappendCompat
  ( (<>)
  ) where
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid ((<>))
#endif
