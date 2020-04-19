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

itemsAsInlineKeyboard :: UserMode -> EditMessage
itemsAsInlineKeyboard Mode { root = o, items = ss } = case ss of
  []    -> "No stations yet"
  items -> (toEditMessage msg)
    { editMessageReplyMarkup = Just
      $ SomeInlineKeyboardMarkup (itemsInlineKeyboard items)
    }
    where msg = "User: '" <> getName o <> "' is a member of these stations:"


itemsInlineKeyboard :: [StItem Station] -> InlineKeyboardMarkup
itemsInlineKeyboard items =
  InlineKeyboardMarkup
    $  chunksOf 2 (map itemInlineKeyboardButton items)
    <> [[applyButton]]
  where applyButton = actionButton "Apply" ConfirmApply

itemInlineKeyboardButton :: NamedItem a => StItem a -> InlineKeyboardButton
itemInlineKeyboardButton item = actionButton (prefix <> getName station')
                                             action
 where
  station'         = getName (getStItem item)
  (action, prefix) = case getStatus item of
    Initial -> (RemoveItem . getName $ station', "\x2705 ")
    Added   -> (RemoveItem . getName $ station', "\x2B05 ")
    Removed -> (RestoreItem . getName $ station', "\x274C ")

removeItem :: Text -> Model -> Model
removeItem s Mode { root = o, items = ss } = Mode { root  = o
                                                  , items = foldr remove [] ss
                                                  }
 where
  remove st@(StItem status s') ss'
    | status == Added && s == getName s'   = ss'
    | status == Initial && s == getName s' = StItem Removed s' : ss'
    | otherwise                            = st : ss'
removeItem _ _ = NoMode

addItem :: Text -> Model -> Model
addItem s Mode { root = o, items = ss } =
  Mode { items = StItem Added s : ss, root = o }
addItem _ _ = NoMode

restoreItem :: Text -> Model -> Model
restoreItem s Mode { root = o, items = ss } = Mode
  { root  = o
  , items = foldr restore [] ss
  }
 where
  restore st@(StItem status s') ss'
    | s == getName s' && status == Removed = StItem Initial s : ss'
    | otherwise                            = st : ss'
restoreItem _ _ = NoMode
