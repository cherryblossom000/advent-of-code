#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments, OverloadedStrings, TupleSections #-}

import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.IO as T

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

readInt :: T.Text -> Int
readInt = read . T.unpack

type Board = [[(Int, Bool)]]

play :: [Int] -> [Board] -> [(Board, Int)]
play _ [] = []
play (n:ns) bs = ((,n) <$> winners) <> play ns rest
  where
    bs' = map (map \c@(x, marked) -> if marked then c else (x, x == n)) <$> bs
    (winners, rest) = partition ((||) <$> hasMarkedRow <*> hasMarkedColumn) bs'
    hasMarkedRow = any $ all snd
    hasMarkedColumn ([]:_) = False
    hasMarkedColumn b = all (snd . head) b || hasMarkedColumn (tail <$> b)

score :: (Board, Int) -> Int
score (b, n) = n * sum [x | r <- b, (x, False) <- r]

main :: IO ()
main = do
  (ns':_:bs') <- T.lines <$> T.readFile "input.txt"
  let ns = readInt <$> T.splitOn "," ns'
  let bs = map (map ((,False). readInt) . T.words) . init <$> chunksOf 6 bs'

  let winners = play ns bs

  -- Part 1
  print . score $ head winners
  -- Part 2
  print . score $ last winners
