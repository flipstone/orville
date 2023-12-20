{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main
  ( main
  ) where

import qualified Control.Monad.IO.Class as MIO
import qualified Control.Monad.Reader as Reader
import qualified Orville.PostgreSQL as O

data ApplicationContext =
  ApplicationContext
    { applicationGreeting :: String
    , applicationOrvilleState :: O.OrvilleState
    }

newtype Application a =
  Application (Reader.ReaderT ApplicationContext IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MIO.MonadIO
    )

instance O.HasOrvilleState Application where
  askOrvilleState =
    Application (Reader.asks applicationOrvilleState)

  localOrvilleState f (Application reader) =
    let
      mkLocalContext :: ApplicationContext -> ApplicationContext
      mkLocalContext ctx =
        ctx
          { applicationOrvilleState = f (applicationOrvilleState ctx)
          }
    in
      Application (Reader.local mkLocalContext reader)

getGreeting :: Application String
getGreeting =
  Application (Reader.asks applicationGreeting)

runApplication :: O.ConnectionPool -> String -> Application a -> IO a
runApplication pool greeting (Application io) =
  let
    orvilleState =
      O.newOrvilleState
        O.defaultErrorDetailLevel
        pool

    context =
      ApplicationContext
        { applicationGreeting = greeting
        , applicationOrvilleState = orvilleState
        }
  in
    Reader.runReaderT io context

myApplication :: Application ()
myApplication = do
  greeting <- getGreeting
  MIO.liftIO . putStrLn $ greeting

main :: IO ()
main = do
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "host=localhost user=postgres password=postgres"
          , O.connectionNoticeReporting = O.DisableNoticeReporting
          , O.connectionPoolStripes = O.OneStripePerCapability
          , O.connectionPoolLingerTime = 10
          , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
          }

  runApplication pool "Hello Application" myApplication
