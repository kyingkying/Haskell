{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length $ filter (\(x,y) -> x == y) $ zip c1 c2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map (\x -> (length x)-1) (groupBy (==) (sort (c ++ colors)))

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum $ map (\(x,y)->(min x y)) $ zip (countColors actual) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonexact
                         where exact = exactMatches secret guess
                               nonexact = (matches secret guess) - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonexact) code = (exact == exact2) && (nonexact == nonexact2)
                                                  where (Move _ exact2 nonexact2) = getMove code guess 

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (\code -> isConsistent move code) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = [[x] | x <- colors]
allCodes n = concatMap (\x -> map (\y -> x ++ y ) (allCodes 1)) (allCodes (n-1))

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = helper secret (allCodes $ length secret)

helper :: Code -> [Code] -> [Move]
helper secret codes
  | length codes <= 1 = []
  | otherwise = move : helper secret (filterCodes move codes)
                  where move = getMove secret (head codes)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
