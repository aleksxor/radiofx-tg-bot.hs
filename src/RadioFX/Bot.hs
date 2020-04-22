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
import           RadioFX.Render
import           RadioFX.Items

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

manipulateItems :: Model -> (Item -> Model -> Model) -> Text -> Eff Action Model
manipulateItems model@ItemMode { root = root' } action text = case root' of
  User    _ -> action (Station text) model <# pure RenderModel
  Station _ -> action (User text) model <# pure RenderModel
manipulateItems model _ _ = model <# pure WrongCommand

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
  RemoveItem  item -> manipulateItems model removeItem item
  RestoreItem item -> manipulateItems model restoreItem item

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
      Just ss -> pure . InitUserMode $ ItemMode { root  = User owner'
                                                , items = StItem Initial <$> ss
                                                }
      Nothing -> do
        replyText $ "Could not fetch stations for: " <> owner'
        pure DoNothing
  InitUserMode     model'   -> model' <# pure RenderModel

  -- StationMode
  StartStationMode station' -> model <# do
    members <- liftIO $ getStationMembers station'
    pure . InitStationMode $ ItemMode { root  = Station station'
                                      , items = StItem Initial <$> members
                                      }

  InitStationMode model' -> model' <# pure RenderModel

  -- Render
  RenderModel            -> model <# do
    replyOrEdit $ itemsAsInlineKeyboard model
    pure DoNothing

