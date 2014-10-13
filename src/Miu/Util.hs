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

findSubstring      :: MiuString -> MiuString -> [Int]
findSubstring w s  = evalState kmpAlgorithm $ KmpState s w 0 0

kmpFailureFunction      :: Eq a => Int -> [a] -> Int
kmpFailureFunction 0 _  = 0
kmpFailureFunction 1 _  = 0
kmpFailureFunction i xs | (xs !! kmpFailureFunction (i-1) xs) == (xs !! (i-1)) = 1 + kmpFailureFunction (i-1) xs
                        | otherwise = 0

kmpDelta                :: MiuSymbol -> MiuSymbol -> KmpState -> KmpState
kmpDelta x y s          | x == y    = s { automatonState = 1 + automatonState s
                                        , currentOffset  = 1 + currentOffset s
                                        }
                        | otherwise = case automatonState s of
                                        0 -> s { currentOffset = 1 + currentOffset s }
                                        _ -> s { automatonState = kmpFailureFunction (automatonState s) $ pattern s }

kmpStateIsAccepting     :: KmpState -> Bool
kmpStateIsAccepting s   | automatonState s == (length (pattern s) - 1) = True
                        | currentOffset s == length (sourceString s)   = True
                        | otherwise                                    = False

-- too imperative; refactor in terms of an automaton
kmpAlgorithm :: State KmpState [Int]
kmpAlgorithm = do
  s <- get

  let i = automatonState s
  let j = currentOffset s
  let src = sourceString s
  let pat = pattern s

  if (j == length src) || null src then return [] else do

    modify $ kmpDelta (src !! j) (pat !! i)

    if (src !! j) == (pat !! i) then do
      if i == (length pat - 1) then do
        put $ s { automatonState = 0
                , currentOffset  = j - i + 1
                }
        otherMatches <- kmpAlgorithm
        return $ (j-i):otherMatches
      else kmpAlgorithm
    else do
      kmpAlgorithm
