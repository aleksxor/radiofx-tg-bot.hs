{-# LANGUAGE OverloadedStrings #-}
module Bot where

import           Control.Applicative            ( (<|>) )
import           Telegram.Bot.Simple            ( (<#)
                                                , Eff(..)
                                                , BotApp(..)
                                                )
import           Telegram.Bot.Simple.UpdateParser
                                                ( text
                                                , command
                                                , parseUpdate
                                                )
import           Telegram.Bot.API               ( Update(..) )
import           Telegram.Bot.Simple.Reply      ( replyText )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

data Model = Model
  deriving (Show)

data Action
  = DoNothing
  | Echo Text
  | ShowMessage
  deriving (Show)

bot :: BotApp Model Action
bot = BotApp { botInitialModel = Model
             , botAction       = flip handleUpdate
             , botHandler      = handleAction
             , botJobs         = []
             }

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _model =
  parseUpdate $ Echo <$> text <|> ShowMessage <$ command "start"

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  DoNothing -> pure model
  Echo msg  -> model <# do
    replyText msg
    pure DoNothing
