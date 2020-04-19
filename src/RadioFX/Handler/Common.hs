{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Handler.Common where

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
  btnNo  = actionButton "No" ShowUserMode
