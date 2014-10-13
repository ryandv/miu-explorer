module Miu.System(
    MiuSymbol(..)
  , MiuString
  , miuRuleOne
  , miuRuleTwo
  , miuRuleThree
  , miuRuleFour
  ) where

import Control.Monad.Trans.State

import Data.List

data MiuSymbol = M | I | U deriving(Eq, Show)
type MiuString = [MiuSymbol]

miuRuleOne        :: MiuString -> Maybe MiuString
miuRuleOne (x:[]) | x == I    = Just [I,U]
                  | otherwise = Nothing
miuRuleOne []     = Nothing
miuRuleOne (x:xs) = miuRuleOne xs >>= (\ys -> return $ x:ys)

miuRuleTwo        :: MiuString -> Maybe MiuString
miuRuleTwo (M:xs) = Just $ [M] ++ xs ++ xs
miuRuleTwo _      = Nothing

miuRuleThree      :: MiuString -> Maybe MiuString
miuRuleThree xs   = do
  blockIndex <- findSubstring [I,I,I] xs
  let prefix = take blockIndex xs
  let suffix = drop (blockIndex + 3) xs
  return $ prefix ++ [U] ++ suffix


miuRuleFour       :: MiuString -> Maybe MiuString
miuRuleFour xs    | [U,U] `elem` blocks  = Just $ concat $ delete [U,U] blocks
                  | otherwise            = Nothing where

  blocks :: [MiuString]
  blocks = group xs

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
