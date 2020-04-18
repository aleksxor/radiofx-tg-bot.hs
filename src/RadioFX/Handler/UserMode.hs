{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Handler.UserMode where

import           Data.List.Split                ( chunksOf )

import           Telegram.Bot.Simple            ( EditMessage(..)
                                                , actionButton
                                                , toEditMessage
                                                )
import           Telegram.Bot.API

import           RadioFX.Types

stationsAsInlineKeyboard :: Model -> EditMessage
stationsAsInlineKeyboard model = case stations model of
  []    -> "No stations yet"
  items -> (toEditMessage msg)
    { editMessageReplyMarkup = Just
      $ SomeInlineKeyboardMarkup (stationsInlineKeyboard items)
    }
   where
    msg =
      "User: '" <> getName (owner model) <> "' is a member of these stations:"


stationsInlineKeyboard :: [StItem Station] -> InlineKeyboardMarkup
stationsInlineKeyboard items =
  InlineKeyboardMarkup
    $  chunksOf 2 (map stationInlineKeyboardButton items)
    <> [[applyButton]]
  where applyButton = actionButton "Apply" DoNothing

stationInlineKeyboardButton :: StItem Station -> InlineKeyboardButton
stationInlineKeyboardButton item = actionButton
  (prefix <> getStation station')
  action
 where
  station'         = getStItem item
  (action, prefix) = case getStatus item of
    Initial -> (RemoveUserStation station', "\x2705 ")
    Added   -> (RemoveUserStation station', "\x2B05 ")
    Removed -> (RestoreUserStation station', "\x274C ")

removeUserStation :: Station -> Model -> Model
removeUserStation s UserMode { owner = o, stations = ss } = UserMode
  { owner    = o
  , stations = foldr removeSt [] ss
  }
 where
  removeSt st@(StItem status s') ss'
    | status == Added && s == s'   = ss'
    | status == Initial && s == s' = StItem Removed s' : ss'
    | otherwise                    = st : ss'
removeUserStation _ m = m

addUserStation :: Station -> Model -> Model
addUserStation s UserMode { owner = o, stations = ss } =
  UserMode { stations = StItem Added s : ss, owner = o }
addUserStation _ _ = NoMode

restoreUserStation :: Station -> Model -> Model
restoreUserStation s UserMode { owner = o, stations = ss } = UserMode
  { owner    = o
  , stations = foldr restoreSt [] ss
  }
 where
  restoreSt st@(StItem status s') ss'
    | s == s' && status == Removed = StItem Initial s : ss'
    | otherwise                    = st : ss'
restoreUserStation _ _ = NoMode
