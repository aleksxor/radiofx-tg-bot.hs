{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Items where

import           Data.Text                      ( Text )
import           Telegram.Bot.Simple            ( Eff(..)
                                                , (<#)
                                                )

import           RadioFX.Types

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
  ItemMode { items = ss <> [StItem Added s], root = o }
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

manipulateItems :: Model -> (Item -> Model -> Model) -> Text -> Eff Action Model
manipulateItems model@ItemMode { root = root' } action text = case root' of
  User    _ -> action (Station text) model <# pure RenderModel
  Station _ -> action (User text) model <# pure RenderModel
manipulateItems model _ _ = model <# pure WrongCommand

