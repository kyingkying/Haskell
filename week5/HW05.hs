{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Maybe
import Data.List

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Bits as Bits

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret path1 path2 = do
  dog1 <- BS.readFile path1
  dog2 <- BS.readFile path2
  let xorlist = map (uncurry Bits.xor) (zip (BS.unpack dog1) (BS.unpack dog2))
  return $ BS.pack $ filter (/=0) xorlist

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  encfile <- BS.readFile (path ++ ".enc")
  let xorlist = map (uncurry Bits.xor) (zip (BS.unpack encfile) (cycle $ BS.unpack $ key))
  BS.writeFile path $ BS.pack $ filter (/=0) xorlist

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  file <- BS.readFile path
  return $ decode file

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vpath tpath = do
  victims <- parseFile vpath :: IO (Maybe [TId])
  transactions <- parseFile tpath :: IO (Maybe [Transaction])
  let badTShelper x y = case (x,y) of
                          (Just vic, Just tran) -> Just $ filter (\t -> elem (tid t) vic) tran
                          (_, _) -> Nothing
  return $ badTShelper victims transactions

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (tran:rest) = Map.insertWith (+) (from tran) (negate $ amount tran) $ Map.insertWith (+) (to tran) (amount tran) (getFlow rest)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ maximumBy compair (Map.toList m)
 
compair :: (String, Integer) -> (String, Integer) -> Ordering
compair (_,b) (_,d) = compare b d

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = undoTshelper (head payers) payees tids [] where
                list = Map.toList m
                payers = sortBy (flip compair) $ filter (\(_, v) -> v>0) list
                payees = sortBy compair $ filter (\(_, v) -> v<0) list

undoTshelper :: (String, Integer) -> [(String, Integer)] -> [TId] -> [Transaction] -> [Transaction]
undoTshelper _ [] _ trans = trans
undoTshelper (_,0) _ _ trans = trans
undoTshelper _ _ [] trans = trans
undoTshelper (name1, val1) ((name2, val2):restpayees) (i:restid) trans
  | val1 >= -val2 = undoTshelper (name1, val1 + val2) restpayees restid (consTrans name1 name2 (-val2) i trans)
  | val1 < -val2 = undoTshelper (name1, 0) ((name2, val1 + val2):restpayees) restid (consTrans name1 name2 val1 i trans)

consTrans :: String -> String -> Integer -> TId -> [Transaction] -> [Transaction]
consTrans n m v i trans= (Transaction {from=n, to=m, amount=v, tid=i}):trans

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path trans = do
  BS.writeFile path $ encode $ trans

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

