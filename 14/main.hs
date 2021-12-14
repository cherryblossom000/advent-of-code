#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow   ((&&&))
import Data.Bifunctor  (bimap)
import Data.Foldable   (traverse_)
import Data.List       qualified as L
import Data.Map.Strict qualified as M
import Data.Text       qualified as T
import Data.Text.IO    qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

type Pair = (Char, Char)
type Rules = M.Map Pair Char
type PairsCounts = M.Map Pair Int
type Input = (PairsCounts, Char, Rules)

parse :: T.Text -> Input
parse s =
    ( M.fromList $ (head &&& length) <$> L.group (L.sort $ T.zip t (T.tail t))
    , T.last t
    , M.fromList
       $  bimap (toTuple . T.unpack) T.head . toTuple . T.splitOn " -> "
      <$> rs
    )
  where t:_:rs = T.lines s

add :: (Num a, Ord k) => k -> a -> M.Map k a -> M.Map k a
add = M.insertWith (+)

step :: Rules -> PairsCounts -> PairsCounts
step rs m = M.foldrWithKey f m m
  where
    f p@(a, b) x = add (c, b) x . add (a, c) x . M.adjust (subtract x) p
      where c = rs M.! p

nTimes :: Int -> (a -> a) -> a -> a
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

solution :: Int -> Input -> Int
solution n (m, l, rs) = last xs - head xs
  where
    xs =  L.sort
       $  snd
      <$> M.toList (add l 1 . M.mapKeysWith (+) fst $ nTimes n (step rs) m)

part1 :: Input -> Int
part1 = solution 10

part2 :: Input -> Int
part2 = solution 40

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
