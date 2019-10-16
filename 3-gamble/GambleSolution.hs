{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GambleSolution where

import           Control.Monad.State
import           System.Random

main :: IO ()
main = do
  r <- game 10
  if r
    then putStrLn "win"
    else putStrLn "lose"

-- A very stupid game:
--  throw two n-sided dices,
--  if the first is larger than the second, you win
--  otherwise, you lose
game :: Integer -> IO Bool
game n = do
  x <- randomElement [1 .. n]
  y <- randomElement [1 .. n]
  return (x > y)

-- Get a random element from a list
randomElement :: [a] -> IO a
randomElement [] = error "Empty lists are not supported"
randomElement xs
       -- generate a random index
 = do
  ix <- randomRIO (0, length xs - 1)
  return (xs !! ix)

class Monad m =>
      GambleMonad m
  where
  roll :: [a] -> m a

-- EXERCISE 1. Rewrite `game` using `GambleMonad`
game' :: GambleMonad m => Integer -> m Bool
game' n = do
  x <- roll [1 .. n]
  y <- roll [1 .. n]
  return (x > y)

-- EXERCISE 2. Write the following instances of `GambleMonad`
instance GambleMonad IO where
  roll = randomElement

-- TreeOfPossibilities records every possibility
data TreeOfPossibilities a
  = Result a
  | ChooseBetween [TreeOfPossibilities a]
  deriving (Eq, Show, Functor)

-- TreeOfPossibilities is a monad!
instance Applicative TreeOfPossibilities where
  pure = Result
  f <*> x = do
    f' <- f
    x' <- x
    return (f' x')

instance Monad TreeOfPossibilities where
  return = Result
  Result x >>= f = f x
  ChooseBetween xs >>= f = ChooseBetween [x >>= f | x <- xs]

instance GambleMonad TreeOfPossibilities where
  roll xs = ChooseBetween (map Result xs)

-- Scripted returns the given results for the rolls
type Scripted = State [Int]

instance GambleMonad Scripted where
  roll xs = do
    values <- get
    let ix = head values `mod` length xs
    put (tail values)
    return (xs !! ix)

-- EXERCISE 3. Write the `UserChoiceMonad` class
class Monad m =>
      UserChoiceMonad m
  where
  chooseFrom :: Show a => [a] -> m a

factorGame :: (GambleMonad m, UserChoiceMonad m) => Integer -> Integer -> m Bool
factorGame bound factors = do
  x <- chooseFrom [1 .. bound]
  y <- roll [1 .. factors]
  return $ x `mod` y == 0
