#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings, TupleSections #-}

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)

type Line = ((Int, Int), (Int, Int))

isVertical :: Line -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

isHorizontal :: Line -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

solution :: [Line] -> Int
solution = M.size
         . M.filter (>1)
         . L.foldl' (flip $ M.unionWith (+) . M.fromList . map (,1) . f) M.empty
  where
    f l@((x1, y1), (x2, y2)) | isVertical   l = (x1,) <$> range y1 y2
                             | isHorizontal l = (,y1) <$> range x1 x2
                             | otherwise      = zip (range x1 x2) (range y1 y2)
    range a b = [a, a + signum (b - a)..b]

main :: IO ()
main = do
  input <-
    map
      ( toTuple
      . map (toTuple . map (read . T.unpack) . T.splitOn ",")
      . T.splitOn " -> "
      )
    .   T.lines
    <$> T.readFile "input.txt"
  -- Part 1
  print . solution $ filter ((||) <$> isVertical <*> isHorizontal) input
  -- Part 2
  print . solution $ input
