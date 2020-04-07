{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Bot where

import           Control.Applicative            ( (<|>) )
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser
import           Telegram.Bot.API
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

data Model = Model
  deriving (Show)

data Action
  = DoNothing
  | Echo Text
  | WelcomeMessage
  | ShowStationGroups Text
  deriving (Show)

bot :: BotApp Model Action
bot = BotApp { botInitialModel = Model
             , botAction       = flip handleUpdate
             , botHandler      = handleAction
             , botJobs         = []
             }

startMessage :: [Text]
startMessage = ["Hello. I'm RadioFX Bot. ", "", ""]

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _model =
  parseUpdate
    $   WelcomeMessage
    <$  command "start"
    <|> ShowStationGroups
    <$> command "show"

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  DoNothing -> pure model
  Echo msg  -> model <# do
    replyText msg
    pure DoNothing
  WelcomeMessage -> model <# do
    replyText (Text.unlines startMessage)
    pure DoNothing
  ShowStationGroups owner -> model <# do
    replyText $ "Show station groups for: " <> owner
    pure DoNothing
