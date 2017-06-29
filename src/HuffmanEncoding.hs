{-# LANGUAGE GADTs #-}

module HuffmanEncoding
    ( frequencies
    , encode
    , decode
    , fsToTrees
    , fsToCoding
    , findBits
    , buildTree
    , insertTree
    , Bit (..)
    , Tree (..)
    ) where

import           Data.List

data Bit = Z | O deriving (Eq, Show)

data Tree a where
    Leaf :: a -> Int -> Tree a
    Node :: Tree a -> Tree a -> Int -> Tree a
    deriving (Show)

instance Eq (Tree a) where
    t1 == t2 = f t1 == f t2

instance Ord (Tree a) where
    compare t1 t2 = compare (f t1) (f t2)

f :: Tree a -> Int
f (Leaf _ x) = x
f (Node _ _ x) = x

fsToTrees :: [(a, Int)] -> [Tree a]
fsToTrees = map $ uncurry Leaf

-- param trees should be sorted
-- param trees should not be empty
buildTree :: [Tree a] -> Tree a
buildTree [] = error "param tree should not be empty"
buildTree [tree] = tree
buildTree (t1:t2:rest) =
    let node = Node t1 t2 (f t1 + f t2) in
        buildTree $ insertTree node rest

insertTree :: Tree a -> [Tree a] -> [Tree a]
insertTree = insertBy (\t1 t2 -> compare (f t1) (f t2))

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies l = map inner $ group $ sort l
    where
        inner :: [a] -> (a, Int)
        inner lst = (head lst, length lst)

_treeToCoding :: Ord a => Tree a -> [Bit] -> [(a, [Bit])]
_treeToCoding (Leaf x _) curBits = [(x, curBits)]
_treeToCoding (Node l r _) curBits =
    _treeToCoding l (curBits ++ [Z]) ++ _treeToCoding r (curBits ++ [O])

treeToCoding tree = _treeToCoding tree []

fsToCoding :: Ord a => [(a, Int)] -> [(a, [Bit])]
fsToCoding fs = treeToCoding $ buildTree $ sort (fsToTrees fs)

findCoding :: Eq a => [(a, [Bit])] -> a -> Maybe [Bit]
findCoding [] x = Nothing
findCoding ((a, bits):rest) x = if a == x then Just bits else findCoding rest x

findBits :: [(a, [Bit])] -> [Bit] -> Maybe (a, [Bit])
findBits [] bits = Nothing
findBits ((x, bs):fs) bits = if bs `isPrefixOf` bits then Just (x, bs) else findBits fs bits

seqMaybe :: [Maybe a] -> Maybe [a]
seqMaybe = foldr seq' $ Just []

seq' :: Maybe a -> Maybe [a] -> Maybe [a]
seq' Nothing _ = Nothing
seq' _ Nothing = Nothing
seq' (Just x) (Just l) = Just $ x : l

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode [] _ = Nothing
encode [_] _ = Nothing
encode _ [] = Just []
encode fs str =
    if length codings <= 1
        then Nothing
        else fmap concat seq
    where
        codings = fsToCoding fs
        seq = seqMaybe $ map (findCoding codings) str

-- | Decode a bit sequence using the given frequencies.
decode :: Ord a => [(a, Int)] -> [Bit] -> Maybe [a]
decode [] _ = Nothing
decode [_] _ = Nothing
decode _ [] = Just []
decode fs bits =
    _decode codings bits []
    where
        codings = fsToCoding fs

_decode :: [(a, [Bit])] -> [Bit] -> [a] -> Maybe [a]
_decode _ [] result = Just result
_decode codings bits result = case x of
    Nothing -> Nothing
    Just (xx, bs) -> _decode codings (deleteFirstsBy (==) bits bs) (result ++ [xx])
    where
        x = findBits codings bits

--decode :: [(a, Int)] -> [Bit] -> Maybe [a]
--decode [] _ = Nothing
--decode [_] _ = Nothing
--decode _ [] = Just []
--decode fs bits = _decode tree tree bits []
--    where
--        tree = buildTree $ fsToTrees fs
--
--_decode ::  Tree a -> Tree a -> [Bit] -> [a] -> Maybe [a]
--_decode _ (Leaf x _) [] result = Just $ result ++ [x]
--_decode tree (Leaf x _) bits result = _decode tree tree bits $ result ++ [x]
--_decode _ (Node _ _ _) [] result = Nothing
--_decode tree (Node l r _) (b:rest) result = case b of
--    Z -> _decode tree l rest result
--    O -> _decode tree r rest result
