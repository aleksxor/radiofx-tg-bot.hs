{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Bot where

import           Control.Applicative            ( (<|>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Telegram.Bot.Simple            ( BotApp(..)
                                                , Eff(..)
                                                , (<#)
                                                , replyText
                                                )
import           Telegram.Bot.Simple.UpdateParser
                                                ( command
                                                , parseUpdate
                                                )
import           Telegram.Bot.API               ( Update(..) )

data Model = Model
  deriving (Show)

data Action
  = DoNothing
  | WelcomeMessage
  | User Text
  | Station Text
  | ArgumentExpected
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
  , "/user <owner@email.com> - show owner's station group(s)"
  , "/station <stationGroup> - show stationGroup members"
  , ""
  , "There are two modes:"
  , "  * User mode - show user's stations and inline keyboard"
  , "    to add/remove user's stations"
  , "  * Station mode - show station group members and commands"
  , "    to add/remove members to the group"
  ]

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _model =
  parseUpdate
    $   WelcomeMessage
    <$  command "start"
    <|> singleArg User
    <$> command "user"
    <|> singleArg Station
    <$> command "station"
    <|> pure WrongCommand
 where
  singleArg :: (Text -> Action) -> Text -> Action
  singleArg action t = case Text.words t of
    [arg] -> action arg
    _     -> ArgumentExpected

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  DoNothing      -> pure model
  WelcomeMessage -> model <# do
    replyText startMessage
    pure DoNothing
  User owner -> model <# do
    replyText $ "Show station groups for: " <> owner
    pure DoNothing
  Station group -> model <# do
    replyText $ "Show group: '" <> group <> "' members"
    pure DoNothing
  WrongCommand -> model <# do
    replyText "ERR: Unsupported command"
    pure DoNothing
  ArgumentExpected -> model <# do
    replyText "ERR: Command expects exactly one argument"
    pure DoNothing
