module LuhnCheck(luhnCheck) where

import Prelude

import Data.Array (head, length, reverse, tail)
import Data.Int (odd)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Math (remainder)

-- |        Luhn Algo 
-- |
-- | Reverse the card number
-- |
-- | `sum <- 0`
-- |
-- |    if `odd_position` then
-- |
-- |      `sum += mulitplies then num[i] by 2 and mod by 9`
-- |
-- |    else 
-- |
-- |      `sum += num[i]`
-- |
-- | check it is divisible by 10

-- Card Luhn check
luhnCheck :: String -> Boolean
luhnCheck cardNumber = 
  let ref = reverse $ flip split (replaceSpace cardNumber) $ Pattern ""
      sum = luhnSum 0 ref in
  (remainder sum 10.0) == 0.0 && (length ref /= 0) && sum /= 0.0
  
-- Ignores Spaces
replaceSpace :: String -> String
replaceSpace = replaceAll (Pattern " ") (Replacement "")

luhnSum :: Int -> Array String -> Number
luhnSum _ [] = 0.0
luhnSum i xs = 
  let firstNumber = fromMaybe 0.0 $ fromString $ fromMaybe "0" $ head xs in
  if odd i
    then (remainder (2.0 * firstNumber) 9.0) + (luhnSum (i+1) $ fromMaybe [] $ tail xs)
    else firstNumber + (luhnSum (i+1) $ fromMaybe [] $ tail xs)
