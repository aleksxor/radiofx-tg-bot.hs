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
removeItem s m@Model { items = ss } = m { items = foldr remove [] ss }
 where
  remove st@(StItem status s') ss'
    | status == Added && s == s'   = ss'
    | status == Initial && s == s' = StItem Removed s' : ss'
    | otherwise                    = st : ss'

addItem :: Item -> Model -> Model
addItem s m@Model { root = o, items = ss } =
  m { items = ss <> [StItem Added s], root = o }

restoreItem :: Item -> Model -> Model
restoreItem s m@Model { items = ss } = m { items = foldr restore [] ss }
 where
  restore st@(StItem status s') ss'
    | s == s' && status == Removed = StItem Initial s : ss'
    | otherwise                    = st : ss'

manipulateItems :: Model -> (Item -> Model -> Model) -> Text -> Eff Action Model
manipulateItems m@Model { root = root' } action text = case root' of
  Just (User    _) -> action (Station text) m <# pure RenderModel
  Just (Station _) -> action (User text) m <# pure RenderModel
  Nothing          -> m <# pure WrongCommand

