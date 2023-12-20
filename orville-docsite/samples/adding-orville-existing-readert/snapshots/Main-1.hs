{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main
  ( main
  ) where

import qualified Control.Monad.IO.Class as MIO
import qualified Control.Monad.Reader as Reader

data ApplicationContext =
  ApplicationContext
    { applicationGreeting :: String
    }

newtype Application a =
  Application (Reader.ReaderT ApplicationContext IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MIO.MonadIO
    )

getGreeting :: Application String
getGreeting =
  Application (Reader.asks applicationGreeting)

runApplication :: String -> Application a -> IO a
runApplication greeting (Application io) =
  let
    context =
      ApplicationContext
        { applicationGreeting = greeting
        }
  in
    Reader.runReaderT io context

myApplication :: Application ()
myApplication = do
  greeting <- getGreeting
  MIO.liftIO . putStrLn $ greeting

main :: IO ()
main =
  runApplication "Hello Application" myApplication
