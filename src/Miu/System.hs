module Miu.System(
    MiuSymbol(..)
  , MiuString
  , miuRuleOne
  , miuRuleTwo
  , miuRuleThree
  , miuRuleFour
  ) where

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
miuRuleThree xs   = replaceBlock blocks where

  blocks :: [MiuString]
  blocks = group xs

  replaceBlock    :: [MiuString] -> Maybe MiuString
  replaceBlock ys = do
    blockIndex <- elemIndex [I,I,I] ys
    let splits = splitAt blockIndex ys
    return $ concat (fst splits) ++ [U] ++ concat (tail $ snd splits)

miuRuleFour       :: MiuString -> Maybe MiuString
miuRuleFour xs    | [U,U] `elem` blocks  = Just $ concat $ delete [U,U] blocks
                  | otherwise            = Nothing where

  blocks :: [MiuString]
  blocks = group xs
