module RadioFX.Types where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
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
  = Root { getRootItem :: Item }
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
  | AuthException Text

instance Show ApiException where
  show ModeException        = "Wrong command for this mode"
  show (AuthException user) = "Failed to authorize user" <> Text.unpack user

instance Exception ApiException

data Confirm
  = Confirm
  | NoConfirm
  deriving (Show, Read)

data Action
  = DoNothing
  | WelcomeMessage
  -- Common
  | Auth Text Text
  | AddItem Text
  | RemoveItem Text
  | RestoreItem Text
  | RenderModel Confirm Model
  | Rerender
  | ReplyError Text
  | ConfirmApply
  | ApplyChanges
  -- Errors
  | ArgumentExpected
  | TwoArgumentsExpected
  | WrongCommand
  -- User Mode
  | StartUserMode Text
  -- Station Mode
  | StartStationMode Text
  deriving (Show, Read)

