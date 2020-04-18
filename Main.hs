{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Telegram.Bot.API               ( defaultTelegramClientEnv
                                                , updateChatId
                                                )
import           Telegram.Bot.Simple            ( getEnvToken
                                                , startBot_
                                                , conversationBot
                                                )
import           Telegram.Bot.Simple.Debug      ( traceBotDefault )

import           RadioFX.Bot                    ( bot )

main :: IO ()
main = do
  botToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  -- apiToken <- getEnvToken "RFX_API_TOKEN"
  env      <- defaultTelegramClientEnv botToken
  startBot_ (traceBotDefault (conversationBot updateChatId bot)) env
