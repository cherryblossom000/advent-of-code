#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow ((>>>))
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List     qualified as L
import Data.Map      qualified as M
import Data.Maybe    (fromMaybe)
import Data.Text     qualified as T
import Data.Text.IO  qualified as T

infixr 1 >>$
(>>$) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f >>$ g = f >>> fmap g

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

type Entry = ([String], [String])
type Input = [Entry]

parse :: T.Text -> Input
parse = T.lines
    >>$ (
          T.splitOn " | "
      >>$ T.words
      >>$ T.unpack
    ) >>> toTuple

part1 :: Input -> Int
part1 = length . filter (`elem` [2, 4, 3, 7]) . (>>= map length . snd)

segments2Digits :: M.Map String Int
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

computeEntry :: Entry -> Int
computeEntry (patterns, output) = L.foldl' (\acc d -> acc * 10 + d) 0
  $ (segments2Digits M.!) . L.sort . map convertSignal <$> output
  where
    convertSignal =
      ((fromMaybe (error "no valid pairings") . L.find isValid $ allMaps) M.!)
    allMaps = M.fromList
      <$> allPairings ['a'..'g'] ['a'..'g']
    isValid m = maybe False
        (((==) `on` length) <*> L.nub)
      $ traverse ((segments2Digits M.!?) . L.sort . map (m M.!)) patterns

part2 :: Input -> Int
part2 = sum . map computeEntry

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  let allInputs = parse <$> [sample1, sample2, input]
  traverse_ (print . part1) allInputs
  putStrLn ""
  traverse_ (print . part2) allInputs

-- cspell: disable
sample1 :: T.Text
sample1 = T.unlines
  [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
  , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
  , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
  , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
  , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
  , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
  , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
  , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
  , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
  , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]

sample2 :: T.Text
sample2 = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
-- cspell: enable
