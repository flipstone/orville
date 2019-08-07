{-|
Module    : Database.Orville.Oracle.Internal.Expr.NameExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE CPP #-}

module Database.Orville.Oracle.Internal.MappendCompat
  ( (<>)
  ) where
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid ((<>))
#endif
