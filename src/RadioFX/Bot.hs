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
                                                , replyOrEdit
                                                , editUpdateMessage
                                                )
import           Telegram.Bot.Simple.UpdateParser
                                                ( UpdateParser(..)
                                                , command
                                                , parseUpdate
                                                , callbackQueryDataRead
                                                )
import           Telegram.Bot.API               ( Update(..) )

import           RadioFX.Types
import           RadioFX.API
import           RadioFX.Handler.Common
import           RadioFX.Handler.UserMode
-- import           RadioFX.Handler.StationMode

bot :: BotApp Model Action
bot = BotApp { botInitialModel = NoMode
             , botAction       = flip handleUpdate
             , botHandler      = handleAction
             , botJobs         = []
             }

orCommand :: Text -> UpdateParser Text
orCommand cmd = command cmd <|> command (cmd <> "@RadioFXServiceBot")

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _model =
  parseUpdate
    $   WelcomeMessage
    <$  orCommand "start"
    <|> singleArg StartUserMode
    <$> orCommand "user"
    <|> singleArg StartStationMode
    <$> orCommand "station"
    <|> singleArg AddItem
    <$> orCommand "add"
    <|> callbackQueryDataRead
    <|> pure WrongCommand
 where
  singleArg :: (Text -> Action) -> Text -> Action
  singleArg action t = case Text.words t of
    [arg] -> action arg
    _     -> ArgumentExpected

manipulateItems
  :: Model -> (a -> ItemMode b a -> ItemMode b a) -> Eff Action Model
manipulateItems model action item = case model of
  UserMode items' ->
    UserMode (action (Station item) items') <# pure ShowUserMode
  StationMode items' ->
    StationMode (action (User item) items') <# pure ShowUserMode
  _ -> model <# pure (WrongModeAction)

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  -- Common Actions
  DoNothing      -> pure model
  WelcomeMessage -> model <# do
    replyText startMessage
    pure DoNothing
  ConfirmApply -> model <# do
    editUpdateMessage (confirmActions model)
    pure DoNothing

  AddItem     item -> manipulateItems model addItem item
  RemoveItem  item -> removeItem item model <# pure ShowUserMode
  RestoreItem item -> restoreItem item model <# pure ShowUserMode

  -- Errors
  WrongCommand     -> model <# do
    replyText "ERR: Unsupported command"
    pure DoNothing
  WrongModeAction action' -> model <# do
    replyText $ "Invalid command for current mode: " <> Text.pack (show action')
    pure DoNothing
  ArgumentExpected -> model <# do
    replyText "ERR: Command expects exactly one argument"
    pure DoNothing

  -- UserMode
  StartUserMode owner' -> model <# do
    mStations <- liftIO $ getUserStations owner'
    case mStations of
      Just ss ->
        pure
          . InitUserMode
          . UserMode
          $ (ItemMode { root = User owner', items = StItem Initial <$> ss })
      Nothing -> do
        replyText $ "Could not fetch stations for: " <> owner'
        pure DoNothing
  InitUserMode model' -> model' <# pure ShowUserMode
  ShowUserMode        -> model <# do
    replyOrEdit $ itemsAsInlineKeyboard model
    pure DoNothing

  -- StationMode
  StartStationMode station' ->
    StationMode (ItemMode { root = Station station', items = [] }) <# do
      replyText $ "Show group: '" <> station' <> "' members"
      pure DoNothing
  ShowStationMode -> pure model
