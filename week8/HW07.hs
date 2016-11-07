{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
  a <- ma
  return $ f a

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = 
  v !? j >>= \x ->
    v !? i >>= \y ->
      return $ (//) v [(i, x), (j, y)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence $ map f as 

getElts :: [Int] -> Vector a -> Maybe [a]
getElts l v = mapM ((!?) v) l

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  i <- getRandomR (0, length v - 1)
  return $ (!?) v i

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n rg = V.replicateM n $ getRandomR rg

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = case (null v) of
              True -> do return V.empty
              False -> do
                r <- getRandomR (0,length v - 1)
                vs <- shuffle (V.init (v // [(r, V.last v)]))
                return $ V.snoc vs (v ! r)
          

{--shuffle v = do
  if null v
    then return V.empty
    else do
      r <- getRandomR (0,length v - 1)
      vs <- shuffle ((V.init v) // [(r, V.last v)])
      return $ V.snoc vs (v ! r)--}
-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = case V.partition (< (v!i)) vv of
                    (v1, v2) -> (v1, v!i, v2)
                  where vv = (V.take i v) V.++ (V.drop (i+1) v)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v = if null v
            then V.empty
            else case partitionAt v 0 of
                  (v1, pivot, v2) -> (qsort v1) V.++ (V.fromList [pivot]) V.++ (qsort v2)


-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v = if null v
            then do return V.empty
            else do
              i <- getRandomR (0, length v -1)
              case partitionAt v i of
                (v1, pivot, v2) -> (qsortR v1) >>= \vec1 ->
                                      (qsortR v2) >>= \vec2 ->
                                          return (vec1 V.++ (V.fromList [pivot]) V.++ vec2)

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v = if null v
              then do return Nothing
              else do
                j <- getRandomR (0, length v -1)
                case partitionAt v j of
                  (v1, pivot, v2) -> if i < length v1
                                      then select i v1
                                      else if i == length v1
                                            then return (Just pivot)
                                            else select (i - length v1 -1) v2

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = do
            label <- labels
            suit <- suits
            return $ Card label suit

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck = deck !? 0 >>= \h ->
                  return (h, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck = if n > 0
                    then nextCard deck >>= \(c, d) ->
                                            getCards (n-1) d >>= \(cards, dd) ->
                                                                    return (c:cards, dd)
                    else Just ([], deck)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
