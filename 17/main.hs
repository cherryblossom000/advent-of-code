#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Foldable (traverse_)
import Data.Text     qualified as T
import Data.Text.IO  qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

type Position = (Int, Int)
type Velocity = (Int, Int)
type State = (Position, Velocity)
type Target = ((Int, Int), (Int, Int))

parse :: T.Text -> Target
parse = toTuple
      . map (toTuple . map (read . T.unpack) . T.splitOn ".." . T.drop 2)
      . T.splitOn ", "
      . T.drop (T.length "target area: ")

step :: State -> State
step ((sx, sy), (vx, vy)) = ((sx + vx, sy + vy), (vx - signum vx, vy - 1))

inTarget :: Target -> Position -> Bool
inTarget (tx, ty) (x, y) = f x tx && f y ty
  where f a (l, h) = a >= l && a <= h

launch :: Target -> Velocity -> Bool
launch t@((_, maxX), (minY, _)) = go . ((0, 0),)
  where
    go s | inTarget t p         = True
         | x > maxX || y < minY = False
         | otherwise            = go s'
      where s'@(p@(x, y), _) = step s

velocities :: Target -> [Velocity]
velocities ((_, maxX), (minY, _)) = [(x, y) | x <- [0..maxX], y <- [minY..200]]

{-
v(t) = u - t
x(t) = \sum_{i=0}^{t-1} v(i) = t(2u + 1 - t) / 2
x'(t) = 0 => t = (2u + 1) / 2
x((2u + 1) / 2) = (2u + 1)^2 / 8
-}
part1 :: Target -> Int
part1 t = maximum
        $  (\(_, vy) -> (2 * vy + 1) ^ (2 :: Int) `div` 8)
       <$> filter (launch t) (velocities t)

part2 :: Target -> Int
part2 t = length . filter id $ launch t <$> velocities t

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  let inputs = parse <$> ["target area: x=20..30, y=-10..-5", input]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
