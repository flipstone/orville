module EntityWrapper.Data.Entity
  ( Entity(..)
  ) where

data Entity key fields = Entity
  { entityKey :: key
  , entityFields :: fields
  } deriving (Show, Eq)
