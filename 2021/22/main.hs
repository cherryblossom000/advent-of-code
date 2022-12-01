#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Foldable   (foldl', traverse_)
import Data.Map.Strict qualified as M
import Data.Text       qualified as T
import Data.Text.IO    qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

data Step = Step Bool (Int, Int) (Int, Int) (Int, Int) deriving Show
type Input = [Step]

parse :: T.Text -> Input
parse = map f . T.lines
  where
    f s =
      let [b, s'] = T.words s
          [xs, ys, zs] = toTuple . map (read . T.unpack) . T.splitOn ".." . T.drop 2 <$> T.splitOn "," s'
      in Step (b == "on") xs ys zs

type Point = (Int, Int, Int)
type Reactor = M.Map Point Bool

step :: Reactor -> Step -> Reactor
step m (Step b xs ys zs) = foldl' (\m' p -> M.insert p b m') m [(x, y, z) | x <- f xs, y <- f ys, z <- f zs]
  where
    f (a1, a2) = [max a1 (-50) .. min a2 50]

part1 :: Input -> Int
part1 = M.size . M.filter id . foldl' step M.empty

part2 :: Input -> Int
part2 x = _

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile) ["sample1.txt", "sample2.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
