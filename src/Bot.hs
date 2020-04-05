module Bot where

import Telegram.Bot.Simple (BotApp(..), Eff(..))
import Telegram.Bot.API (Update(..))

data Model = Model
  deriving (Show)

data Action
  = DoNothing
  deriving (Show)

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate = undefined

handleAction :: Action -> Model -> Eff Action Model
handleAction _ model = pure model
