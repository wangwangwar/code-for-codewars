module ValidBraces
    ( validBraces
    ) where

import           Data.List

-- validBraces( "(){}[]" ) => returns true
-- validBraces( "(}" ) => returns false
-- validBraces( "[(])" ) => returns false
-- validBraces( "([{}])" ) => returns true

validBraces :: String -> Bool
validBraces s = _validBraces s ""

_validBraces :: String -> String -> Bool
_validBraces [] [] = True
_validBraces [] _ = False
_validBraces (x:xs) stack
    | x `elem` "([{" = _validBraces xs (stack ++ [x])
    | x `elem` ")]}" && null stack = False
    | x == ')' && last stack == '(' = _validBraces xs (dropLast stack)
    | x == ']' && last stack == '[' = _validBraces xs (dropLast stack)
    | x == '}' && last stack == '{' = _validBraces xs (dropLast stack)
    | otherwise = False

dropLast :: [a] -> [a]
dropLast xs = take (length xs - 1) xs
