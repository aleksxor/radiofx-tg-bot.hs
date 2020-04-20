{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Handler.UserMode where

import           Data.Text                      ( Text )
import           Data.List.Split                ( chunksOf )

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
import           RadioFX.Handler.Common

itemsAsInlineKeyboard :: Model -> EditMessage
itemsAsInlineKeyboard ItemMode { root = o, items = ss } = case ss of
  []     -> "No stations yet"
  items' -> (toEditMessage msg)
    { editMessageReplyMarkup = Just
      $ SomeInlineKeyboardMarkup (itemsInlineKeyboard items')
    }
   where
    msg = case o of
      User    name -> "User: '" <> name <> "' is a member of these stations:"
      Station name -> "Station: '" <> name <> "' has these members:"
itemsAsInlineKeyboard _ = toEditMessage "ERR: Wrong mode"


itemsInlineKeyboard :: [StItem] -> InlineKeyboardMarkup
itemsInlineKeyboard items' =
  InlineKeyboardMarkup
    $  chunksOf 2 (map itemInlineKeyboardButton items')
    <> [[applyButton]]
  where applyButton = actionButton "Apply" ConfirmApply

itemInlineKeyboardButton :: StItem -> InlineKeyboardButton
itemInlineKeyboardButton item = actionButton (prefix <> station') action
 where
  station'         = getItemName (getStItem item)
  (action, prefix) = case getStatus item of
    Initial -> (RemoveItem station', "\x2705 ")
    Added   -> (RemoveItem station', "\x2B05 ")
    Removed -> (RestoreItem station', "\x274C ")

