{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Items where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Telegram.Bot.Simple            ( EditMessage(..)
                                                , actionButton
                                                , toEditMessage
                                                )
import           Telegram.Bot.API               ( SomeReplyMarkup
                                                  ( SomeInlineKeyboardMarkup
                                                  )
                                                , InlineKeyboardMarkup(..)
                                                , InlineKeyboardButton(..)
                                                )

import           RadioFX.Types

startMessage :: Text
startMessage = Text.unlines
  [ "Hello. I'm RadioFX Bot. "
  , ""
  , "I can help you to add new single- and multi-stations:"
  , ""
  , "Supported commands are:"
  , "/start - show this help"
  , "/user <owner@email.com> - show owner's station group(s)"
  , "/station <stationGroup> - show stationGroup members"
  , "/add <station|user> - add a station or a user depending on the mode"
  , ""
  , "There are two modes:"
  , "  * User mode - show user's stations and inline keyboard"
  , "    to add/remove user's stations"
  , "  * Station mode - show station group members and commands"
  , "    to add/remove members to the group"
  ]

confirmActions :: Model -> EditMessage
confirmActions model = (toEditMessage "Apply")
  { editMessageReplyMarkup = Just
                             . SomeInlineKeyboardMarkup
                             . InlineKeyboardMarkup
                             $ [[btnYes, btnNo]]
  }
 where
  btnYes = actionButton "Yes" DoNothing
  btnNo  = actionButton "No" RenderModel

getItemName :: Item -> Text
getItemName (User    name) = name
getItemName (Station name) = name

removeItem :: Item -> Model -> Model
removeItem s ItemMode { root = o, items = ss } = ItemMode
  { root  = o
  , items = foldr remove [] ss
  }
 where
  remove st@(StItem status s') ss'
    | status == Added && s == s'   = ss'
    | status == Initial && s == s' = StItem Removed s' : ss'
    | otherwise                    = st : ss'
removeItem _ model = model

addItem :: Item -> Model -> Model
addItem s ItemMode { root = o, items = ss } =
  ItemMode { items = StItem Added s : ss, root = o }
addItem _ model = model

restoreItem :: Item -> Model -> Model
restoreItem s ItemMode { root = o, items = ss } = ItemMode
  { root  = o
  , items = foldr restore [] ss
  }
 where
  restore st@(StItem status s') ss'
    | s == s' && status == Removed = StItem Initial s : ss'
    | otherwise                    = st : ss'
restoreItem _ model = model
