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
    <|> singleArg (AddUserStation . Station)
    <$> orCommand "add"
    <|> callbackQueryDataRead
    <|> pure WrongCommand
 where
  singleArg :: (Text -> Action) -> Text -> Action
  singleArg action t = case Text.words t of
    [arg] -> action arg
    _     -> ArgumentExpected

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  -- Common Actions
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

  -- UserMode
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

  -- StationMode
  StartStationMode station' ->
    StationMode { station = Station station', members = [] } <# do
      replyText $ "Show group: '" <> station' <> "' members"
      pure DoNothing
  AddStationMember    _ -> pure model
  RemoveStationMember _ -> pure model
