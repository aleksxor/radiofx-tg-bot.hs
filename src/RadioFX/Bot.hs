{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Bot where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Trans            ( liftIO )
import           Control.Exception              ( catch )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Telegram.Bot.Simple            ( BotApp(..)
                                                , Eff(..)
                                                , (<#)
                                                , replyText
                                                , replyOrEdit
                                                , toEditMessage
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
bot = BotApp
  { botInitialModel = Model { jwt = Nothing, root = Nothing, items = [] }
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
    <|> twoArgs Auth
    <$> orCommand "auth"
    <|> callbackQueryDataRead
    <|> pure WrongCommand
 where
  singleArg :: (Text -> Action) -> Text -> Action
  singleArg action t = case Text.words t of
    [arg] -> action arg
    _     -> ArgumentExpected
  twoArgs :: (Text -> Text -> Action) -> Text -> Action
  twoArgs action t = case Text.words t of
    [arg1, arg2] -> action arg1 arg2
    _            -> TwoArgumentsExpected

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  -- Common Actions
  DoNothing    -> pure model
  ReplyErr err -> model <# do
    replyText $ "ERR: " <> err
    pure DoNothing
  WelcomeMessage -> model <# do
    replyText startMessage
    pure DoNothing
  ConfirmApply -> model <# do
    editUpdateMessage (confirmActions model)
    pure DoNothing
  ApplyChanges -> model <# case jwt model of
    Nothing -> do
      replyOrEdit
        $ toEditMessage "Authorize with /auth command to commit changes"
      pure DoNothing
    Just jwt' -> do
      liftIO $ setUserStations jwt' (root model) (items model) `catch` handle
      pure DoNothing
   where
    handle :: ModeException -> IO ()
    handle = undefined

  AddItem     item -> manipulateItems model addItem item
  RemoveItem  item -> manipulateItems model removeItem item
  RestoreItem item -> manipulateItems model restoreItem item

  -- Errors
  WrongCommand     -> model <# do
    pure $ ReplyErr "Unsupported command"
  WrongModeAction action' -> model <# do
    replyText $ "Invalid command for current mode: " <> Text.pack (show action')
    pure DoNothing
  ArgumentExpected -> model <# do
    pure $ ReplyErr "Command expects exactly one argument"
  TwoArgumentsExpected -> model <# do
    pure $ ReplyErr "Command expects two arguments"

  -- Authorization
  Auth login password -> model <# do
    liftIO $ authorize login password
    pure DoNothing

  -- UserMode
  StartUserMode owner' -> model <# do
    mStations <- liftIO $ getUserStations owner'
    case mStations of
      Just ss -> pure . InitUserMode $ model { root  = Just (User owner')
                                             , items = StItem Initial <$> ss
                                             }
      Nothing -> do
        replyText $ "Could not fetch stations for: " <> owner'
        pure DoNothing
  InitUserMode     model'   -> model' <# pure RenderModel

  -- StationMode
  StartStationMode station' -> model <# do
    members <- liftIO $ getStationMembers station'
    pure . InitStationMode $ model { root  = Just (Station station')
                                   , items = StItem Initial <$> members
                                   }

  InitStationMode model' -> model' <# pure RenderModel

  -- Render
  RenderModel            -> model <# do
    replyOrEdit $ itemsAsInlineKeyboard model
    pure DoNothing

