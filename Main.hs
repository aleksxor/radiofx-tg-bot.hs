{-# LANGUAGE OverloadedStrings #-}
module Main where

import           RadioFX.Bot                    ( bot )

import           Telegram.Bot.API               ( defaultTelegramClientEnv )
import           Telegram.Bot.Simple            ( getEnvToken
                                                , startBot_
                                                )
import           Telegram.Bot.Simple.Debug      ( traceBotDefault )

main :: IO ()
main = do
  botToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  -- apiToken <- getEnvToken "RFX_API_TOKEN"
  env      <- defaultTelegramClientEnv botToken
  startBot_ (traceBotDefault bot) env
