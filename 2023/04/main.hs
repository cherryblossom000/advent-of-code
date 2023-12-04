#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _ = error "expected 2 elements"

-- | (winning numbers, numbers on card)
type Card = (S.Set Int, [Int])

type Input = [Card]

readInt :: T.Text -> Int
readInt = read . T.unpack

parse :: T.Text -> Input
parse =
  map
    ( first S.fromList
        . toTuple
        . map (map readInt . T.words)
        . T.splitOn " | "
        . last
        . T.splitOn ": "
    )
    . T.lines

matches :: Card -> Int
matches (winning, present) = length $ filter (`S.member` winning) present

part1 :: Input -> Int
part1 = sum . map (points . matches)
 where
  points 0 = 0
  points n = 2 ^ (n - 1)

part2 :: Input -> Int
part2 = go . map (1,)
 where
  go [] = 0
  go ((count, card) : xs) =
    count
      + go (zipWith (first . (+)) (replicate (matches card) count <> repeat 0) xs)

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
