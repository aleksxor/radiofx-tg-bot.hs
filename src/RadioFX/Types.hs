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
  } deriving (Show, Read, Eq)

data Item
  = User Text
  | Station Text
  deriving (Show, Read, Eq)

newtype Root
  = Root Item
  deriving (Show, Read)

newtype Jwt
  = Jwt Text
  deriving (Show, Read)

data Model
  = Model
  { jwt :: Maybe Jwt
  , root :: Maybe Root
  , items :: [StItem]
  } deriving (Show, Read)

data ApiException
  = ModeException
  | AuthException
  deriving (Show)

instance Exception ApiException

data Action
  = DoNothing
  | WelcomeMessage
  -- Common
  | Auth Text Text
  | AddItem Text
  | RemoveItem Text
  | RestoreItem Text
  | RenderModel Model
  | ReplyError Text
  | ConfirmApply
  | ApplyChanges
  -- Errors
  | ArgumentExpected
  | TwoArgumentsExpected
  | WrongCommand
  | WrongModeAction Action
  -- User Mode
  | StartUserMode Text
  -- Station Mode
  | StartStationMode Text
  deriving (Show, Read)

