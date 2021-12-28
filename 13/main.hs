#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments, ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Bifunctor (first, second)
import Data.Char      qualified as C
import Data.Foldable  (traverse_)
import Data.List      qualified as L
import Data.Set       qualified as S
import Data.Text      qualified as T
import Data.Text.IO   qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

data Axis = X | Y deriving (Eq, Read, Show)
data Fold = Fold Axis Word deriving Show

type Point = (Word, Word)
type Input = (S.Set Point, [Fold])

readText :: Read a => T.Text -> a
readText = read . T.unpack

parse :: T.Text -> Input
parse t = (S.fromList $ parsePoint <$> points, parseFold <$> folds)
  where
    (points, folds) = toTuple . map T.lines $ T.splitOn "\n\n" t
    parsePoint = toTuple . map readText . T.splitOn ","
    parseFold s = Fold (read . pure . C.toUpper $ T.head s') (readText $ T.drop 2 s')
      where s' = T.drop (T.length "fold along ") s

fold :: S.Set Point -> Fold -> S.Set Point
fold ps (Fold ax n) = as <> S.map ((if ax == X then first else second) ((2 * n) -)) bs
  where
    (as, bs) = S.partition ((<n) . if ax == X then fst else snd) ps

part1 :: Input -> Int
part1 (ps, fs) = S.size . fold ps $ head fs

part2 :: Input -> String
part2 (ps, fs) = unlines
  [ [if (x, y) `S.member` ps' then '#' else ' ' | y <- [0..maximum $ S.map snd ps']]
  | x <- [0..maximum $ S.map fst ps']
  ]
  where ps' = L.foldl' fold ps fs

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  putStrLn . unlines $ part2 <$> inputs
