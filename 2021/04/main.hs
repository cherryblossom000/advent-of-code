#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Data.List (unfoldr, partition)
import qualified Data.Text as T
import qualified Data.Text.IO as T

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr \case
  [] -> Nothing
  xs -> Just $ splitAt n xs

readText :: Read a => T.Text -> a
readText = read . T.unpack

type Board = [[(Word, Bool)]]

play :: [Word] -> [Board] -> [(Board, Word)]
play _ [] = []
play (n:ns) bs = ((,n) <$> winners) <> play ns rest
  where
    bs' = map (map \c@(x, marked) -> if marked then c else (x, x == n)) <$> bs
    (winners, rest) = partition ((||) <$> hasMarkedRow <*> hasMarkedColumn) bs'
    hasMarkedRow = any $ all snd
    hasMarkedColumn ([]:_) = False
    hasMarkedColumn b = all (snd . head) b || hasMarkedColumn (tail <$> b)
play _ _ = error "no more numbers"

score :: (Board, Word) -> Word
score (b, n) = n * sum [x | r <- b, (x, False) <- r]

main :: IO ()
main = do
  (ns':_:bs') <- T.lines <$> T.readFile "input.txt"
  let ns = readText <$> T.splitOn "," ns'
  let bs = map (map ((,False). readText) . T.words) . init <$> chunksOf 6 bs'

  let winners = play ns bs

  -- Part 1
  print . score $ head winners
  -- Part 2
  print . score $ last winners
