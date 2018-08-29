module LuhnCheck(luhnCheck) where

import Prelude

import Data.Array (head, length, reverse, tail)
import Data.Int (odd,fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Data.Either (Either(Right,Left), either)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)

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

checkAlpha :: String -> Either String String
checkAlpha cardNumber = do
  regexVal <- regex "^[0-9\\s]*$" noFlags
  if test regexVal cardNumber
    then (Right cardNumber)
    else (Left "Error")

luhnCheck :: String -> Boolean
luhnCheck = either (const false) (luhnCheck_ <<< reverse <<< split (Pattern "") <<< replaceAll (Pattern " ") (Replacement "")) <<< checkAlpha
  where
    luhnCheck_ ref =
      let sum = luhnSum 0 ref
      in (mod sum 10) == 0 && (length ref /= 0) && sum /= 0

luhnSum :: Int -> Array String -> Int
luhnSum _ [] = 0
luhnSum i xs = 
  let firstNumber = fromMaybe 0 $ fromString =<< head xs in
  if odd i
    then (mod (2 * firstNumber) 9) + (fromMaybe 0 (luhnSum (i+1) <$> tail xs))
    else firstNumber + (fromMaybe 0 (luhnSum (i+1) <$> tail xs))
