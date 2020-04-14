module RadioFX.API.Request
  ( module RadioFX.API.Types
  , getUserStations
  , addUserStation
  , removeUserStation
  )
where

import           Network.HTTP.Simple            ( httpBS
                                                , getResponseBody
                                                )
import           Network.HTTP.Client            ( parseRequest )
import           Control.Lens                   ( preview )
import           Data.Aeson.Lens                ( key
                                                , _String
                                                )
import qualified Data.ByteString.Char8         as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           RadioFX.API.Types

baseURL :: Text
baseURL = "https://api.radiofx.co/users"

fetchJSON :: Text -> IO BS.ByteString
fetchJSON uri = do
  req <- parseRequest . Text.unpack $ baseURL <> uri
  res <- httpBS req
  return $ getResponseBody res

splitGroups :: Text -> [Station]
splitGroups = fmap Station . Text.splitOn ","

getUserStations :: Text -> IO (Maybe [Station])
getUserStations owner = do
  json <- fetchJSON $ "/metadata?id=" <> owner
  return
    $   splitGroups
    <$> preview (key "data" . key "attributes" . key "stationGroup" . _String)
                json

setUserStations :: [Station] -> IO ()
setUserStations = undefined

addUserStation :: Station -> [Station]
addUserStation = undefined

removeUserStation :: Station -> [Station]
removeUserStation = undefined
