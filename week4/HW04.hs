{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P l1) == (P l2) = length (filter (\(n,m) -> n /= m) coeffipairs) == 0 where
                         coeffipairs = if (length l1) <= (length l2) then zip (l1 ++ zeros) l2 else zip l1 (l2 ++ zeros) where
                           zeros = repeat 0
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P l) = showhelper (fmap (\(c, n) -> let showc = if c == 1 then "" else show c in
                                                  case (c, n) of
                                                   (c, 0) -> show c
                                                   (c, 1) -> showc ++ "x"
                                                   (c, n) -> showc ++ "x^" ++ show n)
                                  (filter (\(cc,nn)-> cc/=0) (reverse (zip l [0..]))))

showhelper :: [String] -> String
showhelper [] = ""
showhelper (s:[]) = s
showhelper (s:ss) = s ++ " + " ++ showhelper ss

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P l1) (P l2) = P (plushelper l1 l2)

plushelper :: Num a => [a] -> [a] -> [a]
plushelper [] [] = []
plushelper l [] = l
plushelper [] l = l
plushelper (x:xs) (y:ys) = (x+y):(plushelper xs ys)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

