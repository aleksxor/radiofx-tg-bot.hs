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

type Owner = Text
type Group = Text

data Action
  = DoNothing
  | WelcomeMessage
  | Show Text
  | Add Owner Group
  | Remove Owner Group
  | TwoArgumentsExpected
  | WrongCommand
  deriving (Show)

bot :: BotApp Model Action
bot = BotApp { botInitialModel = Model
             , botAction       = flip handleUpdate
             , botHandler      = handleAction
             , botJobs         = []
             }

startMessage :: Text
startMessage = Text.unlines
  [ "Hello. I'm RadioFX Bot. "
  , ""
  , "I can help you to add new single- and multi-stations:"
  , ""
  , "Supported commands are:"
  , "/show <owner@email.com> - show owner's station group(s)"
  , "/add <owner@email.com> <group> - add owner's radio to the station group"
  , "/remove <owner@email.com> <group> - remove owner's radio from the station group"
  ]

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _model =
  parseUpdate
    $   WelcomeMessage
    <$  command "start"
    <|> Show
    <$> command "show"
    <|> twoArgs Add
    <$> command "add"
    <|> twoArgs Remove
    <$> command "remove"
    <|> pure WrongCommand
 where
  twoArgs :: (Text -> Text -> Action) -> Text -> Action
  twoArgs action t = case Text.words t of
    [arg1, arg2] -> action arg1 arg2
    _            -> TwoArgumentsExpected

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  DoNothing      -> pure model
  WelcomeMessage -> model <# do
    replyText startMessage
    pure DoNothing
  Show owner -> model <# do
    replyText $ "Show station groups for: " <> owner
    pure DoNothing
  Add owner group -> model <# do
    replyText $ "Add group: " <> group <> " to the '" <> owner <> "' station."
    pure DoNothing
  Remove owner group -> model <# do
    replyText
      $  "Remove group: "
      <> group
      <> " from the '"
      <> owner
      <> "' station."
    pure DoNothing
  WrongCommand -> model <# do
    replyText "ERR: Unsupported command"
    pure DoNothing
  TwoArgumentsExpected -> model <# do
    replyText "ERR: Command expects exactly two arguments"
    pure DoNothing
