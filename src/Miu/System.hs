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

miuRuleThree      :: MiuString -> [MiuString]
miuRuleThree xs   = fmap (replaceBlock xs) (findSubstring [I,I,I] xs) where 

    replaceBlock      :: MiuString -> Int -> MiuString
    replaceBlock ys i = let prefix = take i ys
                            suffix = drop (i + 3) ys in
                               prefix ++ [U] ++ suffix


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

findSubstring      :: MiuString -> MiuString -> [Int]
findSubstring w s  = evalState kmpAlgorithm $ KmpState s w 0 0

kmpFailureFunction      :: Eq a => Int -> [a] -> Int
kmpFailureFunction 0 _  = 0
kmpFailureFunction 1 _  = 0
kmpFailureFunction i xs | (xs !! kmpFailureFunction (i-1) xs) == (xs !! (i-1)) = 1 + kmpFailureFunction (i-1) xs
                        | otherwise = 0

-- too imperative; refactor in terms of an automaton
kmpAlgorithm :: State KmpState [Int]
kmpAlgorithm = do
  s <- get

  let i = automatonState s
  let j = currentOffset s
  let src = sourceString s
  let pat = pattern s

  if (j == length src) || null src then return [] else

    if (src !! j) == (pat !! i) then do
      put $ s { automatonState = i + 1
              , currentOffset  = j + 1
              }
      if i == (length pat - 1) then do
        put $ s { automatonState = 0
                , currentOffset  = j - i + 1
                }
        otherMatches <- kmpAlgorithm
        return $ (j-i):otherMatches
      else kmpAlgorithm
    else
      if i > 0 then do
        put $ s { automatonState = kmpFailureFunction i pat }
        kmpAlgorithm
      else do
        put $ s { currentOffset = j + 1 }
        kmpAlgorithm
