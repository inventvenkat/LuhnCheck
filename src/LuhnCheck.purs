module LuhnCheck where

import Prelude

import Data.Array (head, length, reverse, tail)
import Data.Int (odd)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Math (remainder)

-- card Luhn check
luhnCheck :: String -> Boolean
luhnCheck cardNumber = 
  let ref = reverse $ flip split cardNumber $ Pattern ""
      sum = luhnSum 0 ref in
  (remainder sum 10.0) == 0.0 && (length ref /= 0) && sum /= 0.0
  
-- Ignores Spaces
luhnCheck' :: String -> Boolean
luhnCheck' cardNumber = luhnCheck $ replaceAll (Pattern " ") (Replacement "") cardNumber

luhnSum :: Int -> Array String -> Number
luhnSum _ [] = 0.0
luhnSum i xs = 
  let firstNumber = fromMaybe 0.0 $ fromString $ fromMaybe "0" $ head xs in
  if odd i
    then (remainder (2.0 * firstNumber) 9.0) + (luhnSum (i+1) $ fromMaybe [] $ tail xs)
    else firstNumber + (luhnSum (i+1) $ fromMaybe [] $ tail xs)
