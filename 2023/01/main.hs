#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow ((&&&))
import Data.Bifoldable (bisum)
import Data.Bifunctor (bimap, first)
import Data.Foldable (traverse_)
import Data.Foldable1 (Foldable1 (foldMap1))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Semigroup (First (First, getFirst), Last (Last, getLast))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Text.Read (readMaybe)

type Input = [T.Text]

parse :: T.Text -> Input
parse = T.lines

solution :: (T.Text -> (Int, Int)) -> Input -> Int
solution firstLast = sum . map (bisum . first (* 10) . firstLast)

digits :: T.Text -> [Int]
digits = mapMaybe (readMaybe @Int . pure) . T.unpack

part1 :: Input -> Int
part1 =
  solution
    $ bimap getFirst getLast
    . foldMap1 (First &&& Last)
    . NE.fromList
    . digits

letters :: [(T.Text, Int)]
letters =
  [ ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  ]

replace :: T.Text -> T.Text
replace = foldr (\(s, x) -> (T.replace s (T.pack $ show x) .)) id letters

part2 :: Input -> Int
part2 =
  solution
    ( head
        . digits
        . foldMap replace
        . T.inits
        &&& head
        . digits
        . T.reverse
        . foldMap replace
        . T.tails
    )

main :: IO ()
main = do
  [sample1, sample2, input] <-
    traverse (fmap parse . T.readFile) ["sample1.txt", "sample2.txt", "input.txt"]
  traverse_ (print . part1) [sample1, input]
  putStrLn ""
  traverse_ (print . part2) [sample2, input]
