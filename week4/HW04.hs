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
    show (P l) = showhelper (map change (filter (\(c, _) -> c/= 0) (reverse (zip l [0..])))) where
                       change :: (Num a, Eq a, Show a, Num b, Eq b, Show b) => (a, b) -> String
                       change (cc, nn)
                         | nn == 0 = show cc
                         | nn == 1 = showcc ++ "x"
                         | otherwise = showcc ++ "x^" ++ show nn where
                            showcc = case cc of
                                       1 -> ""
                                       -1 -> "-"
                                       _ -> show cc

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
plushelper (y:ys) (z:zs) = (y+z):(plushelper ys zs)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P l1) (P l2) = foldl plus (P []) (map (\(c, n) -> (P ((replicate n 0) ++ (map (*c) l2)))) (zip l1 [0..]))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P l) = P (fmap negate l)
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P l) num = foldl (+) 0 (map (\(c, n) -> c * num^n) (zip l [0..]))

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 f = deriv f
    nderiv n f = nderiv (n-1) (deriv f)

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a)=> Differentiable (Poly a) where
    deriv (P l)  = (P (tail (map (\(c, n) -> n*c ) (zip l [0..]))))

