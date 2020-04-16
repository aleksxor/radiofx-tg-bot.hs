{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Bot where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Trans            ( liftIO )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.List.Split                ( chunksOf )

import           Telegram.Bot.Simple            ( BotApp(..)
                                                , Eff(..)
                                                , EditMessage(..)
                                                , (<#)
                                                , actionButton
                                                , toEditMessage
                                                , replyText
                                                , replyOrEdit
                                                )
import           Telegram.Bot.Simple.UpdateParser
                                                ( command
                                                , parseUpdate
                                                , callbackQueryDataRead
                                                )
import           Telegram.Bot.API               ( Update(..)
                                                , InlineKeyboardMarkup(..)
                                                , InlineKeyboardButton(..)
                                                , SomeReplyMarkup
                                                  ( SomeInlineKeyboardMarkup
                                                  )
                                                )

import           RadioFX.API.Request

data Status
  = Initial
  | Added
  | Removed
  deriving (Show, Read, Eq)

data StItem a = StItem
  { getStatus :: Status
  , getStItem :: a
  }
  deriving (Show, Read, Eq)

newtype StationUser = StationUser { getName :: Text }
  deriving (Show, Read)

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
  deriving (Show, Read)

data Action
  = DoNothing
  | WelcomeMessage
  -- Errors
  | ArgumentExpected
  | WrongCommand
  | WrongModeAction Action
  -- User Mode
  | StartUserMode Text
  | InitUserMode Model
  | RemoveUserStation Station
  | AddUserStation Station
  | RestoreUserStation Station
  | AddUserStationReply
  | ShowUserMode
  -- Station Mode
  | StartStationMode Text
  | AddStationMember StationUser
  | RemoveStationMember StationUser
  deriving (Show, Read)

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


stationsAsInlineKeyboard :: Model -> EditMessage
stationsAsInlineKeyboard model = case stations model of
  []    -> "No stations yet"
  items -> (toEditMessage msg)
    { editMessageReplyMarkup = Just
      $ SomeInlineKeyboardMarkup (stationsInlineKeyboard items)
    }
   where
    msg =
      "User: '" <> getName (owner model) <> "' is a member of these stations:"


stationsInlineKeyboard :: [StItem Station] -> InlineKeyboardMarkup
stationsInlineKeyboard items =
  InlineKeyboardMarkup
    $  chunksOf 2 (map stationInlineKeyboardButton items)
    <> addButton
 where
  addButton = [[(actionButton "\x2795 Add station" (AddUserStationReply))]]


stationInlineKeyboardButton :: StItem Station -> InlineKeyboardButton
stationInlineKeyboardButton item = actionButton
  (prefix <> getStation station')
  action
 where
  station'         = getStItem item
  (action, prefix) = case getStatus item of
    Initial -> (RemoveUserStation station', "\x2705 ")
    Added   -> (RemoveUserStation station', "\x2B05 ")
    Removed -> (RestoreUserStation station', "\x274C ")

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _model =
  parseUpdate
    $   WelcomeMessage
    <$  command "start"
    <|> singleArg StartUserMode
    <$> command "user"
    <|> singleArg StartStationMode
    <$> command "station"
    <|> callbackQueryDataRead
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

  -- Errors
  WrongCommand -> model <# do
    replyText "ERR: Unsupported command"
    pure DoNothing
  WrongModeAction action' -> model <# do
    replyText $ "Invalid command for current mode: " <> Text.pack (show action')
    pure DoNothing
  ArgumentExpected -> model <# do
    replyText "ERR: Command expects exactly one argument"
    pure DoNothing

  -- User Mode
  StartUserMode owner' -> model <# do
    mStations <- liftIO $ getUserStations owner'
    case mStations of
      Just ss -> pure . InitUserMode $ UserMode
        { owner    = StationUser owner'
        , stations = StItem Initial <$> ss
        }
      Nothing -> do
        replyText $ "Could not fetch stations for: " <> owner'
        pure DoNothing
  InitUserMode   model'   -> model' <# pure ShowUserMode
  AddUserStation station' -> addUserStation station' model <# pure ShowUserMode
  RemoveUserStation station' ->
    removeUserStation station' model <# pure ShowUserMode
  RestoreUserStation station' ->
    restoreUserStation station' model <# pure ShowUserMode
  ShowUserMode -> model <# do
    replyOrEdit $ stationsAsInlineKeyboard model
    pure DoNothing

  -- Station Mode
  StartStationMode station' ->
    StationMode { station = Station station', members = [] } <# do
      replyText $ "Show group: '" <> station' <> "' members"
      pure DoNothing

  AddStationMember    member' -> pure model
  RemoveStationMember member' -> pure model

removeUserStation :: Station -> Model -> Model
removeUserStation s UserMode { owner = o, stations = ss } = UserMode
  { owner    = o
  , stations = foldr removeSt [] ss
  }
 where
  removeSt st@(StItem status s') ss'
    | status == Added && s == s'   = ss'
    | status == Initial && s == s' = StItem Removed s' : ss'
    | otherwise                    = st : ss'
removeUserStation _ m = m

addUserStation :: Station -> Model -> Model
addUserStation s UserMode { owner = o, stations = ss } =
  UserMode { stations = StItem Added s : ss, owner = o }
addUserStation _ _ = NoMode

restoreUserStation :: Station -> Model -> Model
restoreUserStation s UserMode { owner = o, stations = ss } = UserMode
  { owner    = o
  , stations = foldr restoreSt [] ss
  }
 where
  restoreSt st@(StItem status s') ss'
    | s == s' && status == Removed = StItem Initial s : ss'
    | otherwise                    = st : ss'
restoreUserStation _ _ = NoMode
