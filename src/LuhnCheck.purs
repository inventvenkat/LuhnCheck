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
-- | check for alphabets and return false if it has 
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
luhnCheck = either (const false) (luhnCheck_ <<< reverse <<< split (Pattern "") <<< replaceAll (Pattern " ") (Replacement "")) <<< checkRegex "^[0-9\\s]*$"
  where
    luhnCheck_ ref =
      let sum = luhnSum 0 ref
      in (mod sum 10) == 0 && (length ref /= 0) && sum /= 0

luhnSum :: Int -> Array String -> Int
luhnSum _ [] = 0
luhnSum i xs = 
  let firstNumber = fromMaybe 0 $ fromString =<< head xs in
  if odd i
    then (if (2 * firstNumber) > 9 then ((mod (2 * firstNumber) 10) + 1) else (2 * firstNumber)) + (fromMaybe 0 (luhnSum (i+1) <$> tail xs))
    else firstNumber + (fromMaybe 0 (luhnSum (i+1) <$> tail xs))

checkRegex :: String -> String -> Either String String
checkRegex regexStr cardNumber = do
  regexVal <- regex regexStr noFlags
  if test regexVal cardNumber
    then (Right cardNumber)
    else (Left "Error")
