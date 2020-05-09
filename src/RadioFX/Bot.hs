{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Bot where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Trans            ( MonadIO
                                                , liftIO
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Control.Exception              ( catch )

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
handleAction action model@Model { jwt = jwt', root = root', items = items' } =
  case action of
  -- Common Actions
    DoNothing      -> pure model
    WelcomeMessage -> model <# do
      replyText startMessage
      pure DoNothing
    ConfirmApply -> model <# do
      editUpdateMessage (confirmActions model)
      pure DoNothing
    ApplyChanges -> model <# case jwt' of
      Nothing -> do
        replyOrEdit
          $ toEditMessage "Authorize with /auth command to commit changes"
        pure DoNothing
      Just token -> do
        res <-
          liftIO $ setUserStations token root' items' `catch` genericHandler
        case res of
          Right () -> pure $ maybe
            (ReplyError "Empty root field")
            (StartUserMode . getItemName . getRootItem)
            root'
          Left ModeException -> pure $ ReplyError "Wrong command for this mode"
          Left _             -> pure $ ReplyError "Could not apply changes"

    AddItem item -> model <# pure
      (RenderModel model
        { items = maybe items' (addItem items') (mkModelItem item <$> root')
        }
      )
    RemoveItem item -> model <# pure
      (RenderModel model
        { items = maybe items' (removeItem items') (mkModelItem item <$> root')
        }
      )
    RestoreItem item -> model <# pure
      (RenderModel model
        { items = maybe items' (restoreItem items') (mkModelItem item <$> root')
        }
      )

    -- Errors
    ReplyError err -> model <# do
      replyText $ "ERR: " <> err
      pure DoNothing
    WrongCommand -> model <# do
      pure $ ReplyError "Unsupported command"
    ArgumentExpected -> model <# do
      pure $ ReplyError "Command expects exactly one argument"
    TwoArgumentsExpected -> model <# do
      pure $ ReplyError "Command expects two arguments"

    -- Authorization
    Auth login password -> model <# do
      res <- liftIO $ authorize login password `catch` genericHandler
      case res of
        Right (Just jwt'') -> do
          replyText "Succesfully authorized"
          pure $ RenderModel model { jwt = Just $ Jwt jwt'' }
        _ -> pure $ ReplyError "Failed to authorize"

    -- UserMode
    StartUserMode owner' -> model <# do
      mStations <- liftIO $ getUserStations owner' `catch` genericHandler
      case mStations of
        Right (Just ss) -> pure . RenderModel $ model
          { root  = Just $ Root (User owner')
          , items = StItem Initial <$> ss
          }
        Right Nothing -> do
          replyText $ "Could not fetch stations for: " <> owner'
          pure DoNothing
        _ -> pure . ReplyError $ "Failed to get stations for user: " <> owner'

    -- StationMode
    StartStationMode station' -> model <# do
      members <- liftIO $ getStationMembers station'
      pure . RenderModel $ model { root  = Just $ Root (Station station')
                                 , items = StItem Initial <$> members
                                 }

    -- Render
    RenderModel model' -> model' <# do
      replyOrEdit $ maybe (toEditMessage selectModeMessage)
                          (itemsAsInlineKeyboard $ items model')
                          (root model')
      pure DoNothing


genericHandler :: (MonadIO m) => e -> m (Either e a)
genericHandler e = pure $ Left e
