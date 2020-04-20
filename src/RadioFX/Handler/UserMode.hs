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

itemsAsInlineKeyboard :: Model -> EditMessage
itemsAsInlineKeyboard (UserMode ItemMode { root = o, items = ss }) = case ss of
  []     -> "No stations yet"
  items' -> (toEditMessage msg)
    { editMessageReplyMarkup = Just
      $ SomeInlineKeyboardMarkup (itemsInlineKeyboard items')
    }
    where msg = "User: '" <> getName o <> "' is a member of these stations:"
itemsAsInlineKeyboard _ = toEditMessage "ERR: Wrong mode"


itemsInlineKeyboard :: [StItem Station] -> InlineKeyboardMarkup
itemsInlineKeyboard items' =
  InlineKeyboardMarkup
    $  chunksOf 2 (map itemInlineKeyboardButton items')
    <> [[applyButton]]
  where applyButton = actionButton "Apply" ConfirmApply

itemInlineKeyboardButton :: NamedItem a => StItem a -> InlineKeyboardButton
itemInlineKeyboardButton item = actionButton (prefix <> station') action
 where
  station'         = getName (getStItem item)
  (action, prefix) = case getStatus item of
    Initial -> (RemoveItem station', "\x2705 ")
    Added   -> (RemoveItem station', "\x2B05 ")
    Removed -> (RestoreItem station', "\x274C ")

removeItem :: Eq b => b -> ItemMode a b -> ItemMode a b
removeItem s ItemMode { root = o, items = ss } = ItemMode
  { root  = o
  , items = foldr remove [] ss
  }
 where
  remove st@(StItem status s') ss'
    | status == Added && s == s'   = ss'
    | status == Initial && s == s' = StItem Removed s' : ss'
    | otherwise                    = st : ss'

addItem :: b -> ItemMode a b -> ItemMode a b
addItem s ItemMode { root = o, items = ss } =
  ItemMode { items = StItem Added s : ss, root = o }

restoreItem :: Eq b => b -> ItemMode a b -> ItemMode a b
restoreItem s ItemMode { root = o, items = ss } = ItemMode
  { root  = o
  , items = foldr restore [] ss
  }
 where
  restore st@(StItem status s') ss'
    | s == s' && status == Removed = StItem Initial s : ss'
    | otherwise                    = st : ss'
