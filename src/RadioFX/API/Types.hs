module RadioFX.API.Types where

import           Data.Text                      ( Text )

newtype Station = Station {getStation :: Text} deriving (Show, Read, Eq)
