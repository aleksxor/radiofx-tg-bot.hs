{-# LANGUAGE OverloadedStrings #-}
module RadioFX.Handler.Shared where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

startMessage :: Text
startMessage = Text.unlines
  [ "Hello. I'm RadioFX Bot. "
  , ""
  , "I can help you to add new single- and multi-stations:"
  , ""
  , "Supported commands are:"
  , "/user <owner@email.com> - show owner's station group(s)"
  , "/station <stationGroup> - show stationGroup members"
  , ""
  , "There are two modes:"
  , "  * User mode - show user's stations and inline keyboard"
  , "    to add/remove user's stations"
  , "  * Station mode - show station group members and commands"
  , "    to add/remove members to the group"
  ]

