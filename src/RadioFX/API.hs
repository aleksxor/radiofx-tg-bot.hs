{-# LANGUAGE OverloadedStrings #-}
module RadioFX.API where

import           Network.HTTP.Simple            ( httpBS
                                                , getResponseBody
                                                )
import           Network.HTTP.Client            ( RequestBody(..)
                                                , parseRequest
                                                , method
                                                , requestBody
                                                , requestHeaders
                                                )
-- import           Debug.Trace                    ( traceM )
import           Network.HTTP.Types             ( hAuthorization )
import           Control.Lens                   ( preview
                                                , (^..)
                                                )
import           Data.Aeson.Lens                ( key
                                                , values
                                                , _String
                                                )
import           Control.Monad.Trans.Resource   ( MonadThrow
                                                , throwM
                                                )
import           Control.Monad.Trans            ( MonadIO )
import qualified Data.ByteString.Char8         as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Aeson                     ( object
                                                , (.=)
                                                , encode
                                                )

import           RadioFX.Types
import           RadioFX.Items

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

getStationMembers :: Text -> IO [Item]
getStationMembers station = do
  json <-
    fetchJSON
    $  "/list?limit=100&filter=%7B\"stationGroup\":\""
    <> station
    <> "\"%7D"
  pure $ User <$> json ^.. allStations
 where
  allStations =
    key "data" . values . key "attributes" . key "stationEmail" . _String

collectItemNames :: [StItem] -> Text
collectItemNames = Text.intercalate "," . collect
 where
  collect   = fmap (getItemName . getStItem) . filter woRemoved
  woRemoved = (/= Removed) . getStatus

setUserStations
  :: (MonadThrow m, MonadIO m) => Jwt -> Maybe Item -> [StItem] -> m ()
setUserStations (Jwt jwt') (Just (User name)) stations = do
  initReq <- parseRequest . Text.unpack $ baseURL <> "/metadata"
  let req = initReq
        { method         = "PUT"
        , requestHeaders = [(hAuthorization, encodeUtf8 $ "JWT " <> jwt')]
        , requestBody    = RequestBodyLBS $ encode reqObject
        }
      stationGroup = collectItemNames stations
      reqObject    = object
        [ "data" .= object
            [ "id" .= name
            , "type" .= ("user" :: Text)
            , "attributes" .= object ["stationGroup" .= stationGroup]
            ]
        ]
  _ <- httpBS req
  pure ()
setUserStations _ _ _ = throwM ModeException

setStationMembers :: Model -> IO ()
setStationMembers = undefined

authorize :: (MonadThrow m, MonadIO m) => Text -> Text -> m (Maybe Text)
authorize login password = do
  initReq <- parseRequest . Text.unpack $ baseURL <> "/login"
  let req = initReq { method      = "POST"
                    , requestBody = RequestBodyLBS $ encode reqObject
                    }
      reqObject = object
        [ "data" .= object
            [ "id" .= login
            , "type" .= ("user" :: Text)
            , "attributes" .= object ["password" .= password]
            ]
        ]
  res <- httpBS req
  pure $ preview (key "meta" . key "jwt" . _String) $ getResponseBody res
