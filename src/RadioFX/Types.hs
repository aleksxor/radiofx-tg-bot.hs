module RadioFX.Types where

import           Data.Text                      ( Text )

data Status
  = Initial
  | Added
  | Removed
  deriving (Show, Read, Eq)

data StItem = StItem
  { getStatus :: Status
  , getStItem :: Item
  }
  deriving (Show, Read, Eq)

data Item
  = User Text
  | Station Text
  deriving (Show, Read, Eq)

data Model
  = NoMode
  | ItemMode { root :: Item, items :: [StItem] }
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

