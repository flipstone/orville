{-|
Module    : Database.Orville.Oracle.Internal.Monad
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT

'Database.Orville.Oracle.ResourceT' provides 'ResourceT' instance of the Orville typeclasses for situations
where you might need it. In particular, if you are using the conduit library, you
may want to wrap 'ResourceT' around your normal monad stack, in which case you'll need
the 'MonadOrville' instance provided here to use 'selectConduit'.

These instances are not included in the default exports for Orville because the required
either a 'MonadUnliftIO' or 'MonadBaseControl' instance of the monad underlying 'ResourceT',
depending on the version of 'ResourceT' you are using. For resource-1.1.10 and above you
must provide 'MonadUnliftIO' instance. For versions prior to 1.1.10 you must provide a
'MonadBaseControl' instance.

This is required by 'MonadOrville' requires an instance to 'MonadBaseControl' to be defined.
The instance provided here can only use one lifting strategy, one we choose 'MonadUnliftIO'
wherever possible (both by our own opinion and because later versions of 'ResourceT' have
removed 'MonadBaseControl' support). 'MonadBaseControl' is used for versions of 'ResourceT'
before 'ResourceT' supported 'MonadUnliftIO'.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Orville.Oracle.ResourceT
  (
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (ResourceT, transResourceT)
import qualified Database.Orville.Oracle as O
--
#if MIN_VERSION_resourcet(1,1,10)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Database.Orville.Oracle.MonadUnliftIO as OULIO
#else
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Database.Orville.Oracle.MonadBaseControl as OMBC
#endif
--
instance (Monad m, O.HasOrvilleContext conn m) =>
         O.HasOrvilleContext conn (ResourceT m) where
  getOrvilleEnv = lift O.getOrvilleEnv
  localOrvilleEnv modEnv = transResourceT (O.localOrvilleEnv modEnv)
--
#if MIN_VERSION_resourcet(1,1,10)
--
instance (O.MonadOrvilleControl m, MonadUnliftIO m) =>
         O.MonadOrvilleControl (ResourceT m) where
  liftWithConnection = OULIO.liftWithConnectionViaUnliftIO
  liftFinally = OULIO.liftFinallyViaUnliftIO

instance (MonadUnliftIO m, O.MonadOrville conn m) =>
         O.MonadOrville conn (ResourceT m)
--
#else
--
instance (O.MonadOrvilleControl m, MonadBaseControl IO m) =>
         O.MonadOrvilleControl (ResourceT m) where
  liftWithConnection = OMBC.liftWithConnectionViaBaseControl
  liftFinally = OMBC.liftFinallyViaBaseControl

instance (MonadBaseControl IO m, O.MonadOrville conn m) =>
         O.MonadOrville conn (ResourceT m)
--
#endif

--
