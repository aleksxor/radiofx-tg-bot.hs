{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Bot where

import           Control.Applicative            ( (<|>) )
import           Control.Exception              ( catch
                                                , displayException
                                                )
import           Control.Monad.Trans            ( MonadIO
                                                , liftIO
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Telegram.Bot.API               ( Update(..) )
import           Telegram.Bot.Simple            ( BotApp(..)
                                                , Eff(..)
                                                , replyOrEdit
                                                , replyText
                                                , toEditMessage
                                                , (<#)
                                                )
import           Telegram.Bot.Simple.UpdateParser
                                                ( UpdateParser(..)
                                                , callbackQueryDataRead
                                                , command
                                                , parseUpdate
                                                )

import           RadioFX.API                    ( setUserStations
                                                , setStationMembers
                                                , authorize
                                                , getUserStations
                                                , getStationMembers
                                                )
import           RadioFX.Items                  ( getItemName
                                                , addItem
                                                , removeItem
                                                , restoreItem
                                                , mkModelItem
                                                , filterOutRemoved
                                                )
import           RadioFX.Render                 ( startMessage
                                                , selectModeMessage
                                                , itemsAsInlineKeyboard
                                                )
import           RadioFX.Types                  ( Model(..)
                                                , Confirm(..)
                                                , Action
                                                  ( DoNothing
                                                  , WelcomeMessage
                                                  , ApplyChanges
                                                  , ConfirmApply
                                                  , AddItem
                                                  , RemoveItem
                                                  , RestoreItem
                                                  , ReplyError
                                                  , WrongCommand
                                                  , ArgumentExpected
                                                  , TwoArgumentsExpected
                                                  , Auth
                                                  , StartUserMode
                                                  , StartStationMode
                                                  , Rerender
                                                  , RenderModel
                                                  )
                                                , Root(..)
                                                , Jwt(..)
                                                , Status(Initial)
                                                , StItem(..)
                                                , Item(Station, User)
                                                , getStItem
                                                )

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
    <|> Rerender
    <$  orCommand "show"
    <|> callbackQueryDataRead
    <|> pure DoNothing
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
    ConfirmApply -> model <# pure (RenderModel Confirm model)
    ApplyChanges -> model <# case jwt' of
      Nothing -> do
        replyOrEdit
          $ toEditMessage "Authorize with /auth command to commit changes"
        pure DoNothing
      Just token -> case root' of
        Just (Root (User _ _)) -> do
          let stations = getStItem <$> filterOutRemoved items'
          res <-
            liftIO $ setUserStations token root' stations `catch` genericHandler
          case res of
            Right () -> pure $ maybe
              (ReplyError "Empty root field")
              (StartUserMode . getItemName . getRootItem)
              root'
            Left e -> pure . ReplyError . Text.pack $ displayException e
        Just (Root (Station _)) -> do
          res <-
            liftIO $ setStationMembers token root' items' `catch` listHandler
          case res of
            [Left e] -> pure . ReplyError . Text.pack $ displayException e
            _ -> pure $ maybe (ReplyError "Empty root field")
                              (StartStationMode . getItemName . getRootItem)
                              root'
         where
          listHandler :: (MonadIO m) => e -> m [Either e ()]
          listHandler = pure . pure . Left
        Nothing -> pure $ ReplyError "Empty root field"

    AddItem item -> model <# pure
      (RenderModel
        NoConfirm
        model
          { items = maybe items' (addItem items') (mkModelItem item <$> root')
          }
      )
    RemoveItem item -> model <# pure
      (RenderModel
        NoConfirm
        model
          { items = maybe items'
                          (removeItem items')
                          (mkModelItem item <$> root')
          }
      )
    RestoreItem item -> model <# pure
      (RenderModel
        NoConfirm
        model
          { items = maybe items'
                          (restoreItem items')
                          (mkModelItem item <$> root')
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
          pure $ RenderModel NoConfirm model { jwt = Just $ Jwt jwt'' }
        Right Nothing -> pure $ ReplyError "Failed to parse response"
        Left  e       -> pure . ReplyError . Text.pack $ displayException e

    -- UserMode
    StartUserMode owner' -> model <# do
      mStations <- liftIO $ getUserStations owner' `catch` genericHandler
      case mStations of
        Right (Just ss) -> pure . RenderModel NoConfirm $ model
          { root  = Just $ Root (User True owner')
          , items = StItem Initial <$> ss
          }
        Right Nothing -> do
          replyText $ "Could not fetch stations for: " <> owner'
          pure DoNothing
        Left e -> pure . ReplyError . Text.pack $ displayException e

    -- StationMode
    StartStationMode station' -> model <# do
      eMembers <- liftIO $ getStationMembers station' `catch` genericHandler
      case eMembers of
        Right members -> pure . RenderModel NoConfirm $ model
          { root  = Just $ Root (Station station')
          , items = StItem Initial <$> members
          }
        Left e -> pure . ReplyError . Text.pack $ displayException e

    -- Render
    Rerender                   -> model <# pure (RenderModel NoConfirm model)
    RenderModel confirm model' -> model' <# do
      replyOrEdit $ maybe (toEditMessage selectModeMessage)
                          (itemsAsInlineKeyboard confirm $ items model')
                          (root model')
      pure DoNothing


genericHandler :: (MonadIO m) => e -> m (Either e a)
genericHandler = pure . Left
