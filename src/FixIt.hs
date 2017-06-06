module FixIt (
    reverse',
    foldr'
    ) where

import           Prelude hiding (foldr, reverse)

fix :: (a -> a) -> a
fix f = let x = f x in x

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f a = if null a then [] else f (drop 1 a) ++ [head a]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' f fun init a = if null a then init else fun (head a) (f fun init (drop 1 a))
