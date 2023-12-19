{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main
  ( main
  ) where

import qualified Control.Monad.IO.Class as MIO

newtype Application a =
  Application (IO a)
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
