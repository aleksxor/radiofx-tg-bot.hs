{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Bot where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Trans            ( liftIO )
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

import           RadioFX.API.Request

data Status
  = Unchanged
  | Added
  | Removed
  deriving (Show)

data StItem a = StItem Status a
  deriving (Show)

data StationUser = StationUser
  deriving (Show)

data Model
  = NoMode
  | UserMode
    { owner :: StationUser
    , stations :: [StItem Station]
    }
  | StationMode
    { station :: Station
    , members :: [StItem StationUser]
    }
  deriving (Show)

data Action
  = DoNothing
  | WelcomeMessage
  | ShowUser Text
  | ShowStation Text
  | ArgumentExpected
  | WrongCommand
  deriving (Show)

bot :: BotApp Model Action
bot = BotApp { botInitialModel = NoMode
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
    <|> singleArg ShowUser
    <$> command "user"
    <|> singleArg ShowStation
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
  ShowUser user -> model <# do
    mStations <- liftIO $ getUserStations user
    case mStations of
      Just ss -> do
        replyText $ "User: '" <> user <> "' is a member of theese stations:"
        replyText . Text.unlines $ getStation <$> ss
      Nothing -> replyText $ "Could not fetch stations for: " <> user
    pure DoNothing
  ShowStation group -> model <# do
    replyText $ "Show group: '" <> group <> "' members"
    pure DoNothing
  WrongCommand -> model <# do
    replyText "ERR: Unsupported command"
    pure DoNothing
  ArgumentExpected -> model <# do
    replyText "ERR: Command expects exactly one argument"
    pure DoNothing
