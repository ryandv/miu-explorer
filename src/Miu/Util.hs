module Miu.Util(
    findSubstring
  , kmpFailureFunction
  ) where

import Control.Monad.Trans.State

import Miu.System

data KmpState = KmpState
  { sourceString   :: MiuString
  , pattern        :: MiuString
  , automatonState :: Int
  , currentOffset  :: Int
  }

findSubstring      :: MiuString -> MiuString -> Maybe Int
findSubstring w s  = evalState kmpAlgorithm $ KmpState s w 0 0

kmpFailureFunction      :: Eq a => Int -> [a] -> Int
kmpFailureFunction 0 _  = 0
kmpFailureFunction 1 _  = 0
kmpFailureFunction i xs | (xs !! kmpFailureFunction (i-1) xs) == (xs !! (i-1)) = 1 + kmpFailureFunction (i-1) xs
                        | otherwise = 0

-- too imperative; refactor in terms of an automaton
kmpAlgorithm :: State KmpState (Maybe Int)
kmpAlgorithm = do
  s <- get

  let i = automatonState s
  let j = currentOffset s
  let src = sourceString s
  let pat = pattern s

  if j == length src then return Nothing else

    if (src !! j) == (pat !! i) then do
      put $ s { automatonState = i + 1
              , currentOffset  = j + 1
              }
      if i == (length pat - 1) then return $ Just (j - i) else kmpAlgorithm
    else
      if i > 0 then do
        put $ s { automatonState = kmpFailureFunction i pat }
        kmpAlgorithm
      else do
        put $ s { currentOffset = j + 1 }
        kmpAlgorithm

--
--kmpAlgorithm (w:[]) (s:[]) | w == s    = do
--  resultIndex <- get
--  return . Just . fst $ resultIndex
--                           | otherwise = return Nothing
--
--kmpAlgorithm _ []          = return Nothing
--
--kmpAlgorithm (w:ws) (s:ss) | w == s    = do
--  modify 
