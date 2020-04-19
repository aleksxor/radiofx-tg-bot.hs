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

class NamedItem a where
  getName :: a -> Text

newtype Station
  = Station Text
  deriving (Show, Read, Eq)

newtype User
  = User Text
  deriving (Show, Read, Eq)

instance NamedItem User where
  getName (User s) = s

instance NamedItem Station where
  getName (Station s) = s

data Mode a b = Mode { root :: a, items :: [StItem b] }

type UserMode = Mode User Station
type StationMode = Mode Station User

data Model
  = NoMode
  | UserMode
  | StationMode
  deriving (Show, Read)

data Action
  = DoNothing
  | WelcomeMessage
  | AddItem Text
  | RemoveItem Text
  | RestoreItem Text
  | ConfirmApply
  -- Errors
  | ArgumentExpected
  | WrongCommand
  | WrongModeAction Action
  -- User Mode
  | StartUserMode Text
  | InitUserMode Model
  | ShowUserMode
  -- Station Mode
  | StartStationMode Text
  | InitStationMode Model
  | ShowStationMode
  deriving (Show, Read)

