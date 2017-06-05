module Permutations
    ( permutations,
    permutations',
    genPerm,
    listEq,
    ) where

import           Data.List hiding (permutations)

permutations s = nub $ permutations' s

permutations' :: String -> [String]
permutations' [] = [[]]
permutations' [x] = [[x]]
permutations' (x:xs) = concatMap (genPerm x) $ permutations' xs

-- nub
dereplicate :: Ord a => [[a]] -> [[a]]
dereplicate = map (head) . groupBy listEq . sort

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = first ++ x:last
    where (first, last) = splitAt i xs

genPerm :: a -> [a] -> [[a]]
genPerm x xs = genPerm' 0 x xs

genPerm' :: Int -> a -> [a] -> [[a]]
genPerm' i x xs =
    if i >= 0 && i <= length xs
        then
            insertAt i x xs:genPerm' (i + 1) x xs
        else
            [insertAt i x xs]

listEq :: Eq a => [a] -> [a] -> Bool
listEq lstA lstB = length lstA == length lstB && all (uncurry (==)) (zip lstA lstB)
