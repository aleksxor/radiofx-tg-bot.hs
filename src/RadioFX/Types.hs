{-# LANGUAGE FlexibleInstances #-}
module RadioFX.Types where

import           Data.Text                      ( Text )

data Status
  = Initial
  | Added
  | Removed
  deriving (Show, Read, Eq)

data StItem a = StItem
  { getStatus :: Status
  , getStItem :: a
  }
  deriving (Show, Read, Eq)

newtype Station = Station {getStation :: Text} deriving (Show, Read, Eq)

newtype StationUser = StationUser { getName :: Text }
  deriving (Show, Read)

data Model
  = NoMode
  | UserMode
    { owner :: StationUser
    , stations :: [StItem Station]
    }
  | StationMode
    { station :: Station
    , members :: [StItem StationUser]
    }
  deriving (Show, Read)

data Action
  = DoNothing
  | WelcomeMessage
  -- Errors
  | ArgumentExpected
  | WrongCommand
  | WrongModeAction Action
  -- User Mode
  | StartUserMode Text
  | InitUserMode Model
  | RemoveUserStation Station
  | AddUserStation Station
  | RestoreUserStation Station
  | ShowUserMode
  -- Station Mode
  | StartStationMode Text
  | AddStationMember StationUser
  | RemoveStationMember StationUser
  deriving (Show, Read)

