{-# LANGUAGE OverloadedStrings #-}

module Client where

import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromMaybe)
import           Network.Simple.TCP
import           System.Environment    (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let [server, port] = parseArgs args
   in connect server port $ \(socket, _) -> do
        send socket "Test!"
        msg <- recv socket 1000
        B.putStrLn $ fromMaybe "" msg
  where
    parseArgs xs =
      case xs of
        [h, p] -> [h, p]
        _      -> ["127.0.0.1", "8080"]
