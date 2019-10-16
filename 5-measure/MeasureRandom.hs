module MeasureRandom where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad            (replicateM)
import           System.Random

import           Criterion.Main

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

throwCoinList :: Int -> IO Int
throwCoinList n = count id <$> replicateM n randomIO

throwCoinLoop :: Int -> IO Int
throwCoinLoop n = go n 0
  where
    go :: Int -> Int -> IO Int
    go 0 acc = pure acc
    go n acc = do
      x <- randomIO
      go
        (n - 1)
        (if x
           then acc + 1
           else acc)

throwCoinListAsync :: Int -> IO Int
throwCoinListAsync n = return 4

throwCoinSTM :: Int -> IO Int
throwCoinSTM n = return 4

main :: IO ()
main =
  defaultMain
    [ bench "list" $ whnfIO (throwCoinList 10000)
    , bench "loop" $ whnfIO (throwCoinLoop 10000)
    , bench "async" $ whnfIO (throwCoinListAsync 10000)
    , bench "stm" $ whnfIO (throwCoinSTM 10000)
    ]
