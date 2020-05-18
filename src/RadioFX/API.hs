{-# LANGUAGE OverloadedStrings #-}
module RadioFX.API where

import           Network.HTTP.Simple            ( httpBS
                                                , getResponseBody
                                                )
import           Network.HTTP.Client            ( RequestBody(..)
                                                , parseRequest
                                                , parseUrlThrow
                                                , method
                                                , requestBody
                                                , requestHeaders
                                                )
import           Debug.Trace                    ( traceM )
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
import           Control.Monad.Trans            ( liftIO
                                                , MonadIO
                                                )
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

getUserStations :: (MonadIO m, MonadThrow n) => Text -> m (n (Maybe [Item]))
getUserStations owner' = do
  json <- liftIO $ fetchJSON ("/metadata?id=" <> owner')
  pure
    .   pure
    $   splitGroups
    <$> preview (key "data" . key "attributes" . key "stationGroup" . _String)
                json

getStationMembers :: (MonadIO m, MonadThrow n) => Text -> m (n [Item])
getStationMembers station = do
  json <-
    liftIO
    .  fetchJSON
    $  "/list?limit=100&filter=%7B\"stationGroup\":\""
    <> station
    <> "\"%7D"
  pure . pure $ User <$> json ^.. allStations
 where
  allStations =
    key "data" . values . key "attributes" . key "stationEmail" . _String

setUserStations
  :: (MonadIO m, MonadThrow n) => Jwt -> Maybe Root -> [Item] -> m (n ())
setUserStations (Jwt jwt') (Just (Root (User name))) stations = do
  initReq <- liftIO $ parseUrlThrow . Text.unpack $ baseURL <> "/metadadta"
  let req = initReq
        { method         = "PUT"
        , requestHeaders = [(hAuthorization, encodeUtf8 $ "JWT " <> jwt')]
        , requestBody    = RequestBodyLBS $ encode reqObject
        }
      stationGroup = Text.intercalate "," $ getItemName <$> stations
      reqObject    = object
        [ "data" .= object
            [ "id" .= name
            , "type" .= ("user" :: Text)
            , "attributes" .= object ["stationGroup" .= stationGroup]
            ]
        ]
  _ <- httpBS req
  liftIO . traceM . Text.unpack $ name <> " : " <> stationGroup
  pure . pure $ ()
setUserStations _ _ _ = pure $ throwM ModeException

setStationMembers
  :: (MonadIO m, MonadThrow n) => Jwt -> Maybe Root -> [StItem] -> m [n ()]
setStationMembers jwt' (Just (Root s@(Station name))) users = mapM go users
 where
  go :: (MonadIO m, MonadThrow n) => StItem -> m (n ())
  go (StItem Added   u@(User _)) = store u (Station name :)
  go (StItem Removed u@(User _)) = store u (filter (/= s))
  go _                           = pure $ throwM ModeException

  store u@(User u') f = do
    mSs <- getUserStations u'
    case mSs of
      Just (Just ss) -> setUserStations jwt' (Just (Root u)) (f ss)
      _ ->
        pure
          .  throwM
          .  ApiException
          $  "could not fetch stations for user: "
          <> u'
  store _ _ = pure $ throwM ModeException
setStationMembers _ _ _ = pure $ throwM ModeException

authorize :: (MonadIO m, MonadThrow n) => Text -> Text -> m (n (Maybe Text))
authorize login password = do
  initReq <- liftIO $ parseUrlThrow . Text.unpack $ baseURL <> "/login"
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
  pure
    . pure
    . preview (key "meta" . key "jwt" . _String)
    . getResponseBody
    $ res
