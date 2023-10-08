module Test.ReservedWords
  ( reservedWordsTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville

import qualified Test.Entities.User as User
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

reservedWordsTests :: Orville.Pool Orville.Connection -> Property.Group
reservedWordsTests pool =
  Property.group "ReservedWords" $
    [
      ( String.fromString "Can insert and select an entity with reserved words in its schema"
      , Property.singletonProperty $ do
          originalUser <- HH.forAll User.generate

          usersFromDB <-
            MIO.liftIO $ do
              Pool.withResource pool $ \connection ->
                TestTable.dropAndRecreateTableDef connection User.table

              Orville.runOrville pool $ do
                _ <- Orville.insertEntity User.table originalUser
                Orville.findEntitiesBy User.table mempty

          usersFromDB HH.=== [originalUser]
      )
    ]
