{-# LANGUAGE OverloadedStrings #-}
module RadioFX.API where

import           Network.HTTP.Simple            ( httpBS
                                                , httpNoBody
                                                , getResponseBody
                                                )
import           Network.HTTP.Client            ( RequestBody(..)
                                                , parseRequest
                                                , method
                                                , requestBody
                                                , requestHeaders
                                                )
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

setUserStations :: (MonadThrow m, MonadIO m) => Model -> m ()
setUserStations Model { jwt = jwt', root = owner, items = stations } =
  case (jwt', owner) of
    (Just (Jwt jwt''), Just (User name)) -> do
      initReq <- parseRequest . Text.unpack $ baseURL <> "/metadata"
      let req = initReq
            { method         = "POST"
            , requestHeaders = [(hAuthorization, encodeUtf8 $ "JWT " <> jwt'')]
            , requestBody    = RequestBodyLBS $ encode reqObject
            }
          stationGroup = collectItemNames stations
          reqObject    = object
            [ "id" .= name
            , "type" .= ("user" :: Text)
            , "attributes" .= object ["stationGroup" .= stationGroup]
            ]
      _ <- httpNoBody req
      pure ()
    _ -> throwM ModeException


setStationMembers :: Model -> IO ()
setStationMembers = undefined
