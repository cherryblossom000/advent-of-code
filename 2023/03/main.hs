#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad (join)
import Data.Char qualified as C
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S

type Point = (Int, Int)

-- | (string) length, value
type Number = (Int, Int)

type Symbol = Char
type Input = ([(Point, Symbol)], [(Point, Number)])

parse :: String -> Input
parse =
  partitionEithers
    . join
    . zipWith
      ( \j ->
          map
            ( \xs@((i, c) NE.:| _) ->
                let p = (j, i)
                 in if C.isDigit c
                      then Right (p, ((fst (NE.last xs) - i) + 1, read . NE.toList $ snd <$> xs))
                      else Left (p, c)
            )
            . filter ((/= '.') . snd . NE.head)
            . NE.groupBy ((&&) `on` (C.isDigit . snd))
            . zip [0 ..]
      )
      [0 ..]
    . lines

part1 :: Input -> Int
part1 (symbols, numbers) =
  sum
    . map (snd . snd)
    $ filter
      ( \((j, i), (l, _)) ->
          any (`S.member` S.fromList (fst <$> symbols)) $ (j, i - 1)
            : (j, i + l)
            : ((,) <$> [j - 1, j + 1] <*> [i - 1 .. i + l])
      )
      numbers

part2 :: Input -> Int
part2 (symbols, numbers) =
  sum
    . map (product . map (snd . snd))
    . filter ((== 2) . length)
    . map
      ( \((sj, si), _) ->
          filter
            (\((j, i), (l, _)) -> abs (j - sj) <= 1 && i - 1 <= si && si <= i + l)
            numbers
      )
    $ filter ((== '*') . snd) symbols

main :: IO ()
main = do
  inputs <- traverse (fmap parse . readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
