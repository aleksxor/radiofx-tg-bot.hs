module RadioFX.Types where

import           Data.Text                      ( Text )
import           Control.Exception.Base         ( Exception(..) )

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

data ModeException
  = ModeException
  deriving (Show)

instance Exception ModeException

data Action
  = DoNothing
  | WelcomeMessage
  -- Common
  | AddItem Text
  | RemoveItem Text
  | RestoreItem Text
  | RenderModel
  | ConfirmApply
  -- Errors
  | ArgumentExpected
  | WrongCommand
  | WrongModeAction Action
  -- User Mode
  | StartUserMode Text
  | InitUserMode Model
  -- Station Mode
  | StartStationMode Text
  | InitStationMode Model
  deriving (Show, Read)

