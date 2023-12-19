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
    )

runApplication :: Application a -> IO a
runApplication (Application io) =
  io

myApplication :: Application ()
myApplication =
  MIO.liftIO . putStrLn $ "Hello Application"

main :: IO ()
main =
  runApplication myApplication
