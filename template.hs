#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow ((&&&))
import Control.Monad (guard, join, unless, when, (<=<))
import Data.Array qualified as A
import Data.Bifunctor (bimap, first, second)
import Data.Char qualified as C
import Data.Either (fromLeft, fromRight, isLeft, isRight, partitionEithers)
import Data.Foldable (traverse_)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _ = error "expected 2 elements"

type Point = (Int, Int)

type Input = T.Text

readInt :: T.Text -> Int
readInt = read . T.unpack

parse :: T.Text -> Input
parse x = x

part1 :: Input -> Int
part1 x = 0

part2 :: Input -> Int
part2 x = 0

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
