{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main
  ( main
  ) where

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.IO.Class as MIO
import qualified Orville.PostgreSQL as O

newtype Application a =
  Application (Reader.ReaderT O.OrvilleState IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MIO.MonadIO
    , O.MonadOrville
    , O.MonadOrvilleControl
    , O.HasOrvilleState
    )

runApplication :: O.ConnectionPool -> Application a -> IO a
runApplication pool (Application reader) =
  let
    orvilleState =
      O.newOrvilleState
        O.defaultErrorDetailLevel
        pool
  in
    Reader.runReaderT reader orvilleState

myApplication :: Application ()
myApplication =
  MIO.liftIO . putStrLn $ "Hello Application"

main :: IO ()
main =
  runApplication myApplication
