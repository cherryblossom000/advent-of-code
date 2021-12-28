#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List     qualified as L
import Data.Map      qualified as M
import Data.Maybe    (fromMaybe)
import Data.Text     qualified as T
import Data.Text.IO  qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

type Entry = ([String], [String])
type Input = [Entry]

parse :: T.Text -> Input
parse = map (toTuple . map (map T.unpack . T.words) . T.splitOn " | ") . T.lines

part1 :: Input -> Int
part1 = length . filter (`elem` [2, 4, 3, 7]) . (>>= map length . snd)

segments2Digits :: M.Map String Word
segments2Digits = M.fromList
  [ ("abcefg", 0)
  , ("cf", 1)
  , ("acdeg", 2)
  , ("acdfg", 3)
  , ("bcdf", 4)
  , ("abdfg", 5)
  , ("abdefg", 6)
  , ("acf", 7)
  , ("abcdefg", 8)
  , ("abcdfg", 9)
  ]

allPairings :: [a] -> [b] -> [[(a, b)]]
allPairings xs ys = zip xs <$> L.permutations ys

computeEntry :: Entry -> Word
computeEntry (patterns, output) = L.foldl' (\acc d -> acc * 10 + d) 0
  $ (segments2Digits M.!) . L.sort . map convertSignal <$> output
  where
    convertSignal =
      ((fromMaybe (error "no valid pairings") . L.find isValid $ allMaps) M.!)
    allMaps = M.fromList <$> allPairings ['a'..'g'] ['a'..'g']
    isValid m = maybe False
        (((==) `on` length) <*> L.nub)
      $ traverse ((segments2Digits M.!?) . L.sort . map (m M.!)) patterns

part2 :: Input -> Word
part2 = sum . map computeEntry

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile) ["sample1.txt", "sample2.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
