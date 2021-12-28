#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Char     (isLower)
import Data.Foldable (traverse_)
import Data.Set      qualified as S
import Data.Text     qualified as T
import Data.Text.IO  qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

type Node = T.Text
type Edge = (Node, Node)
type Input = [Edge]

isSmall :: Node -> Bool
isSmall = isLower . T.head

parse :: T.Text -> Input
parse = map (toTuple . T.splitOn "-") . T.lines

paths :: Bool -> Input -> Word
paths d es = go S.empty d "start"
  where
    go _ _ "end" = 1
    go visited doubled n = sum $ go visited' doubled' <$> filter p ns
      where
        ns = es >>= \(a, b) -> if a == n then [b] else [a | b == n]
        (visited', doubled') = if isSmall n
          then (S.insert n visited, doubled || S.member n visited)
          else (visited, doubled)
        p x = x /= "start" && not (doubled' && S.member x visited')

part1 :: Input -> Word
part1 = paths True

part2 :: Input -> Word
part2 = paths False

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile)
    ["sample1.txt", "sample2.txt", "sample3.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
