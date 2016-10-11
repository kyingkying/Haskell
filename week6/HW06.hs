{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = [1,1] ++ zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x $ sIterate f (f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) s = Cons x $ sInterleave s xs

sTake :: Int -> Stream a -> [a]
sTake n _ | n <= 0  = []
sTake n (Cons x xs) = x : sTake (n-1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) (fmap (+1) ruler)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = sIterate lcg (lcg seed) where
              lcg :: Int -> Int 
              lcg n = mod (n * 1103515245 + 12345) 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax l = go 2147483648 0 l where
            go mini maxi [] = Just (mini, maxi)
            go mini maxi (x:xs)
              | x < mini = go x maxi xs
              | x < maxi = go mini maxi xs
              | otherwise = go mini x xs

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix a = M a a a a

instance Num a => Num (Matrix a) where
  (M x y z w) + (M xx yy zz ww) = M (x+xx) (y+yy) (z+zz) (w+ww)
  (M x y z w) * (M xx yy zz ww) = M (x*xx+y*zz) (x*yy+y*ww) (z*xx+w*zz) (z*yy+w*ww)
  negate (M x y z w) = M (-x) (-y) (-z) (-w)
  fromInteger n = M (fromInteger n) 0 0 (fromInteger n)
  abs = undefined
  signum = undefined

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib n = case (M 1 1 1 0) ^ n of
              M _ y _ _ -> y
