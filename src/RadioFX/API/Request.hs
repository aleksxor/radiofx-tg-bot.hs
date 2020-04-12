module RadioFX.API.Request where

import           Network.HTTP.Simple            ( httpBS )
import           Control.Lens                   ( preview )
import           Data.Aeson.Lens                ( key
                                                , _String
                                                )
import qualified Data.ByteString.Char8         as BS
import           Data.Text                      ( Text )

import           RadioFX.API.Types

baseUrl :: Text
baseUrl = "https://api.radiofx.co/users"

getUserStations :: Text -> Maybe [Station]
getUserStations = undefined

setUserStations :: [Station] -> IO ()
setUserStations = undefined

addUserStation :: Station -> [Station]
addUserStation = undefined

removeUserStation :: Station -> [Station]
removeUserStation = undefined
