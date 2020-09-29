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

data Visibility
  = Visible
  | Hidden
  deriving (Read, Eq)

instance Show Visibility where
  show Visible = ""
  show Hidden  = "[hidden]"

data Item
  = User Visibility Text
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
  | ApiException Text

instance Show ApiException where
  show ModeException        = "Wrong command for this mode"
  show (AuthException user) = "Failed to authorize user" <> Text.unpack user
  show (ApiException  text) = "Failed to make API request" <> Text.unpack text

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

