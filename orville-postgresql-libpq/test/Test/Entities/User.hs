module Test.Entities.User
  ( User (..),
    table,
    generate,
  )
where

import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville

import qualified Test.PgGen as PgGen

data User = User
  { user :: T.Text
  }
  deriving (Show, Eq)

table :: Orville.TableDefinition Orville.NoKey User User
table =
  Orville.mkTableDefinitionWithoutKey "user" userMarshaller

userMarshaller :: Orville.SqlMarshaller User User
userMarshaller =
  User
    <$> Orville.marshallField user userField

userField :: Orville.FieldDefinition Orville.NotNull T.Text
userField =
  Orville.unboundedTextField "user"

generate :: HH.Gen User
generate =
  User
    <$> PgGen.pgText (Range.constant 0 10)
