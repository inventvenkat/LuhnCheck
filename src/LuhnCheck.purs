module LuhnCheck where

import Prelude

import Data.Array (head, reverse, tail)
import Data.Int (odd, round)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Math (remainder)

luhnCheck :: String -> Boolean
luhnCheck cardNumber = 
  let ref = reverse $ split (Pattern "") (replaceAll (Pattern " ") (Replacement "") cardNumber)
      sum = luhnSum 0.0 ref in
  (remainder sum 10.0) == 0.0
  
luhnSum :: Number -> Array String -> Number
luhnSum _ [] = 0.0
luhnSum i xs = if odd $ round i
  then (remainder (2.0 * (fromMaybe 0.0 $ fromString $ fromMaybe "0" $ head xs)) 9.0) + (luhnSum (i+1.0) $ fromMaybe [] $ tail xs)
  else (fromMaybe 0.0 $ fromString $ fromMaybe "0" $ head xs) + (luhnSum (i+1.0) $ fromMaybe [] $ tail xs)
