#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _ = error "expected 2 elements"

-- | (red, green, blue)
type Handful = (Int, Int, Int)

type Game = (Int, [Handful])
type Input = [Game]

readInt :: T.Text -> Int
readInt = read . T.unpack

parseHandful :: T.Text -> Handful
parseHandful t = (findCube "red", findCube "green", findCube "blue")
 where
  cubes = first readInt . toTuple . T.words <$> T.splitOn ", " t
  findCube c = maybe 0 fst $ L.find ((== c) . snd) cubes

parseGame :: T.Text -> Game
parseGame t =
  ( readInt $ T.drop (T.length "Game ") game
  , parseHandful <$> T.splitOn "; " handfuls
  )
 where
  (game, handfuls) = toTuple $ T.splitOn ": " t

parse :: T.Text -> Input
parse = map parseGame . T.lines

part1 :: Input -> Int
part1 =
  sum . map fst . filter (all (\(r, g, b) -> r <= 12 && g <= 13 && b <= 14) . snd)

part2 :: Input -> Int
part2 = sum . map (product . map (maximum . (0 :)) . (\(a, b, c) -> [a, b, c]) . unzip3 . snd)

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
