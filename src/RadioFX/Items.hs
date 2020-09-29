{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Items where

import           Data.Text                      ( Text )
import           RadioFX.Types

getItemName :: Item -> Text
getItemName (User visible name) = name <> " " <> show visible
getItemName (Station      name) = name

removeItem :: [StItem] -> Item -> [StItem]
removeItem ss s = foldr remove [] ss
 where
  remove st@(StItem status s') ss'
    | status == Added && s == s'   = ss'
    | status == Initial && s == s' = StItem Removed s' : ss'
    | otherwise                    = st : ss'

addItem :: [StItem] -> Item -> [StItem]
addItem ss s = ss <> [StItem Added s]

restoreItem :: [StItem] -> Item -> [StItem]
restoreItem ss s = foldr restore [] ss
 where
  restore st@(StItem status s') ss'
    | s == s' && status == Removed = StItem Initial s : ss'
    | otherwise                    = st : ss'

mkModelItem :: Text -> Root -> Item
mkModelItem text (Root root') = case root' of
  User  _ _ -> Station text
  Station _ -> User Visible text

filterOutRemoved :: [StItem] -> [StItem]
filterOutRemoved = filter ((/= Removed) . getStatus)
