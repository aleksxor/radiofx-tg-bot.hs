{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Items where

import           Data.Text                      ( Text )
import           RadioFX.Types

getItemName :: Item -> Text
getItemName (User    name) = name
getItemName (Station name) = name

removeItem :: Model -> Item -> Model
removeItem m@Model { items = ss } s = m { items = foldr remove [] ss }
 where
  remove st@(StItem status s') ss'
    | status == Added && s == s'   = ss'
    | status == Initial && s == s' = StItem Removed s' : ss'
    | otherwise                    = st : ss'

addItem :: Model -> Item -> Model
addItem m@Model { root = o, items = ss } s =
  m { items = ss <> [StItem Added s], root = o }

restoreItem :: Model -> Item -> Model
restoreItem m@Model { items = ss } s = m { items = foldr restore [] ss }
 where
  restore st@(StItem status s') ss'
    | s == s' && status == Removed = StItem Initial s : ss'
    | otherwise                    = st : ss'

manipulateItems :: Item -> (Item -> Model) -> Text -> Model
manipulateItems root' action text = case root' of
  User    _ -> action (Station text)
  Station _ -> action (User text)
