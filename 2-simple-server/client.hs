{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client where

import           Control.Exception
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromMaybe)
import           Network.Simple.TCP
import           System.Environment    (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [h, p] -> doRealWork h p
    _      -> putStrLn "Wrong arguments!"

doRealWork :: HostName -> ServiceName -> IO ()
doRealWork server port =
  connect server port (\(socket, _) -> do
       send socket "Test!"
       msg <- recv socket 1000
       B.putStrLn $ fromMaybe "" msg)
  `catch` \(e :: SomeException) ->
    putStrLn $ "Caught exception: " ++ show e
