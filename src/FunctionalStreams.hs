module FunctionalStreams
where

import           Control.Applicative
import           Control.Arrow

data Stream a = a :> Stream a deriving (Show)
infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (a :> _) = a

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> s) = s


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS a = a :> repeatS a

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS xs = head xs :> cycleS (tail xs ++ [head xs])

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS n = n :> fromS (n + 1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = x :> fromStepS (x + s) s

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x (foldrS f xs)

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs) = if p x then x :> filterS p xs else filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i s = _takeS i s []
    where
        _takeS i (x :> xs) lst = if i > 0 then _takeS (i - 1) xs (lst ++ [x]) else lst

takeWhileS :: (a -> Bool) -> Stream a -> [a]
takeWhileS p s = _takeWhileS p s []
    where
        _takeWhileS p (x :> xs) lst = if p x
            then _takeWhileS p xs (lst ++ [x])
            else lst


-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i ss@(x :> xs) = if i > 0 then dropS (i - 1) xs else ss

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (takeS i s, dropS i s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure = repeatS

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = _fibS 0 []

_fibS :: Integer -> [Integer] -> Stream Integer
_fibS 0 lst = 0 :> _fibS 1 (0:lst)
_fibS 1 lst = 1 :> _fibS 2 (1:lst)
_fibS n lst = let x = head lst + head (tail lst) in x :> _fibS (n + 1) (x:lst)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = _primeS 2 []

_primeS :: Integer -> [Integer] -> Stream Integer
_primeS 2 lst = 2 :> _primeS 3 [2]
_primeS n lst =
    if all (\x -> n `mod` x /= 0) lst
        then n :> _primeS (n + 2) (n:lst)
        else _primeS (n + 2) lst

