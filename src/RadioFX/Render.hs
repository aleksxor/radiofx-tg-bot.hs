{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Render where

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
import           RadioFX.Items

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
itemInlineKeyboardButton item = actionButton (prefix <> item') action
 where
  item'            = getItemName (getStItem item)
  (action, prefix) = case getStatus item of
    Initial -> (RemoveItem item', "\x2705 ")
    Added   -> (RemoveItem item', "\x2B05 ")
    Removed -> (RestoreItem item', "\x274C ")

