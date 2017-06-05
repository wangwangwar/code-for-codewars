module RangeExtraction
    ( rangeExtraction,
    concatGrp
    )
where

import           Data.List

rangeExtraction :: [Int] -> String
rangeExtraction xs = intercalate "," $ map listToString group
    where
        group = concatGrp xs []
        listToString :: [Int] -> String
        listToString lst = case length lst of
            0 -> ""
            1 -> show (head lst)
            2 -> show (head lst) ++ "," ++ show (last lst)
            otherwise -> show (head lst) ++ "-" ++ show (last lst)


concatGrp :: [Int] -> [[Int]] -> [[Int]]
concatGrp [] result = result
concatGrp (x:xs) result
    | null result || null (last result) || abs (x - last (last result)) > 1
        = concatGrp xs $ result ++ [[x]]
    | otherwise = concatGrp xs $ dropLast result ++ [last result ++ [x]]

dropLast :: [a] -> [a]
dropLast xs = take (length xs - 1) xs
