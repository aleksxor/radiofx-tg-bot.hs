{-# LANGUAGE OverloadedStrings #-}
module RadioFX.API
  ( getUserStations
  , setUserStations
  , getStationMembers
  , setStationMembers
  )
where

import           Network.HTTP.Simple            ( httpBS
                                                , getResponseBody
                                                )
import           Network.HTTP.Client            ( parseRequest )
import           Control.Lens                   ( preview )
import           Data.Aeson.Lens                ( key
                                                , values
                                                , _String
                                                )
import qualified Data.ByteString.Char8         as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           RadioFX.Types

baseURL :: Text
baseURL = "https://api.radiofx.co/users"

fetchJSON :: Text -> IO BS.ByteString
fetchJSON uri = do
  req <- parseRequest . Text.unpack $ baseURL <> uri
  res <- httpBS req
  return $ getResponseBody res

splitGroups :: Text -> [Item]
splitGroups = fmap Station . Text.splitOn ","

getUserStations :: Text -> IO (Maybe [Item])
getUserStations owner' = do
  json <- fetchJSON $ "/metadata?id=" <> owner'
  return
    $   splitGroups
    <$> preview (key "data" . key "attributes" . key "stationGroup" . _String)
                json

getStationMembers :: Text -> IO (Maybe [Item])
getStationMembers station = do
  json <-
    fetchJSON
    $  "/list?limit=100&filter=%7B\"stationGroup\":\""
    <> station
    <> "\"%7D"
  pure $ parseStation json

parseStation = preview (key "data" . values . _String) -- (key "attributes" . key "stationEmail" . _String))


setUserStations :: Model -> IO ()
setUserStations = undefined

setStationMembers :: Model -> IO ()
setStationMembers = undefined
