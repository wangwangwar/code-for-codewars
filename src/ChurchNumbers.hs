module ChurchNumbers (
    churchAdd,
    churchMul,
    churchPow,
    findChurch
) where

type Lambda a = (a -> a)
type Cnum a = Lambda a -> Lambda a

churchAdd :: Cnum a -> Cnum a -> Cnum a
churchAdd c1 c2 = \h -> c1 h . c2 h

churchMul :: Cnum a -> Cnum a -> Cnum a
churchMul c1 c2 = c1 . (\h' -> c2 h')

--Extra credit: Why is the type signature different?
-- f -> g -> f^g
churchPow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
churchPow cb ce = ce cb

zero :: Cnum a
zero f = id

one :: Cnum a
one f = f

churchSucc :: Cnum a -> Cnum a
churchSucc c = \h -> h . c h
-- example:
-- 0: zero
-- 3: succ(succ(succ(zero)))
-- churchSucc 0: \h -> h . zero h

churchify 0 = zero
churchify n = churchSucc (churchify (n-1))

numerify :: Cnum Int -> Int
numerify c = c (+1) 0

findChurch fn x y = numerify $ fn (churchify x) $ churchify y
