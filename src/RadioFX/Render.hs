{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Render where

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
import           RadioFX.Items

startMessage :: Text
startMessage = Text.unlines
  [ "Hello. I'm RadioFX Bot. "
  , ""
  , "I can help you to add new single- and multi-stations:"
  , ""
  , "Supported commands are:"
  , "/start - show this help"
  , "/auth <login> <password> - authenticate as a user"
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
confirmActions m = (toEditMessage "Apply")
  { editMessageReplyMarkup = Just
                             . SomeInlineKeyboardMarkup
                             . InlineKeyboardMarkup
                             $ [[btnYes, btnNo]]
  }
 where
  btnYes = actionButton "Yes" ApplyChanges
  btnNo  = actionButton "No" (RenderModel m)


itemsAsInlineKeyboard :: Model -> EditMessage
itemsAsInlineKeyboard Model { root = o, items = ss } = case o of
  Nothing ->
    "ERR: Undecidable mode. Please start with /user or /station command."
  Just (User name) ->
    itemKeyboard $ "User: '" <> name <> "' is a member of these stations:"
  Just (Station name) ->
    itemKeyboard $ "Station: '" <> name <> "' has these members:"
 where
  itemKeyboard msg = case ss of
    []     -> "Has no members"
    items' -> (toEditMessage msg)
      { editMessageReplyMarkup = Just
        $ SomeInlineKeyboardMarkup (itemsInlineKeyboard items')
      }

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

