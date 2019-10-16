{-# LANGUAGE OverloadedStrings #-}

module V1SayHello where

import           Data.Maybe         (fromMaybe)
import           Network.Simple.TCP

main :: IO ()
main =
  serve (Host "127.0.0.1") "8080" $ \(socket, _) -> do
    name <- recv socket 1000
    send socket $ "Hello, " <> fromMaybe "" name <> "!"
