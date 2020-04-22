{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Render where

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
    $  map (pure . itemInlineKeyboardButton) items'
    <> [[applyButton]]
 where
  applyButton = actionButton btnText ConfirmApply
  hasStatus st = length . filter (== st) . fmap getStatus
  added   = hasStatus Added items'
  removed = hasStatus Removed items'
  btnText = case (added, removed) of
    (0, 0) -> "Nothing changed"
    (0, r) -> "Apply (remove " <> Text.pack (show r) <> ")"
    (a, 0) -> "Apply (add " <> Text.pack (show a) <> ")"
    (a, r) ->
      "Apply (add "
        <> Text.pack (show a)
        <> ", remove "
        <> Text.pack (show r)
        <> ")"

itemInlineKeyboardButton :: StItem -> InlineKeyboardButton
itemInlineKeyboardButton item = actionButton (prefix <> item') action
 where
  item'            = getItemName (getStItem item)
  (action, prefix) = case getStatus item of
    Initial -> (RemoveItem item', "")
    Added   -> (RemoveItem item', "\x2705 ")
    Removed -> (RestoreItem item', "\x274C ")

