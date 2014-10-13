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


miuRuleFour        :: MiuString -> [MiuString]
miuRuleFour xs     = fmap (removeBlock xs) (findSubstring [U,U] xs) where
  removeBlock      :: MiuString -> Int -> MiuString
  removeBlock ys i = let prefix = take i ys
                         suffix = drop (i + 2) ys in
                           prefix ++ suffix


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

kmpDelta                              :: KmpState -> KmpState
kmpDelta s@(KmpState { automatonState = i
                     , currentOffset  = j
                     , sourceString   = src
                     , pattern        = pat
                     })               | j == length src            = s { automatonState = i - 1 }
                                      | null src || null pat       = s
                                      | (src !! j) == (pat !! i)   = if i == length pat - 1 then s else
                                                                       kmpDelta $ s { automatonState = 1 + i
                                                                                    , currentOffset  = 1 + j
                                                                                    }
                                      | otherwise                  = case i of
                                                                       0 -> kmpDelta $ s { currentOffset = 1 + j }
                                                                       _ -> kmpDelta $ s { automatonState = kmpFailureFunction i pat }

kmpStateIsAccepting     :: KmpState -> Bool
kmpStateIsAccepting s   | automatonState s == (length (pattern s) - 1) = True
                        | otherwise                                    = False

kmpAlgorithm :: State KmpState [Int]
kmpAlgorithm = do
  s <- get

  let finalState = kmpDelta s

  let i = automatonState finalState
  let j = currentOffset finalState
  if kmpStateIsAccepting finalState then do
        put $ finalState { automatonState = 0
                         , currentOffset  = j - i + 1
                         }
        otherMatches <- kmpAlgorithm
        return $ (j-i):otherMatches
  else return []
