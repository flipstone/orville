module ParameterizedEntity.Data.Virus
  ( Virus(..)
  , VirusId(..)
  , VirusName(..)
  , Mutation(..)
  , MutationId(..)
  , bpsVirus
  , brnVirus
  ) where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text

data Virus key = Virus
  { virusId :: key
  , virusName :: VirusName
  } deriving (Show, Eq)

newtype VirusId = VirusId
  { unVirusId :: Int32
  } deriving (Show, Eq)

newtype VirusName = VirusName
  { unVirusName :: Text
  } deriving (Show, Eq)

bpsVirus :: Virus ()
bpsVirus =
  Virus
    { virusId = ()
    , virusName = VirusName (Text.pack "Bovine popular stomachitis")
    }

brnVirus :: Virus ()
brnVirus =
  Virus
    { virusId = ()
    , virusName = VirusName (Text.pack "Black raspberry necrosis")
    }

data Mutation key = Mutation
  { mutationId :: key
  , mutationName :: VirusName
  , mutationParent :: VirusId
  } deriving (Eq, Show)

newtype MutationId = MutationId
  { unMutationId :: Int32
  } deriving (Eq, Show)
