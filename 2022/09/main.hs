#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments, MultiWayIf, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -Wno-tabs #-}

import Data.Bifunctor (bimap, first, second)
import Data.Foldable  (traverse_)
import Data.List      (nub)
import Data.Text      qualified as T
import Data.Text.IO   qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

type Point = (Int, Int)

data Direction = R | U | L | D deriving (Read, Show)

move :: Direction -> Point -> Point
move R = first  (+1)
move D = second (+1)
move L = first  (subtract 1)
move U = second (subtract 1)

type Input = [(Direction, Int)]

parse :: T.Text -> Input
parse = map (bimap read read . toTuple . words . T.unpack) . T.lines . T.strip

moveKnot :: Point -> Point -> Point
moveKnot (hx, hy) t@(tx, ty) =
	let
		dx = hx - tx
		dy = hy - ty
	in if
	| abs dx <= 1 && abs dy <= 1 -> t
	| dx ==  2 && hy == ty -> (tx + 1, ty)
	| dx == -2 && hy == ty -> (tx - 1, ty)
	| dy ==  2 && hx == tx -> (tx, ty + 1)
	| dy == -2 && hx == tx -> (tx, ty - 1)
	| otherwise -> (tx + signum dx, ty + signum dy)

solution :: Int -> Input -> Int
solution n = length
           . nub
           . map last
           . scanl (\(x : xs) dir -> let x' = move dir x in x' : go x' xs) (replicate n (0, 0))
           . (>>= \(dir, x) -> replicate x dir)
	where
		go _ [] = []
		go p1 (p2 : ps) = let p2' = moveKnot p1 p2 in p2' : go p2' ps

part1 :: Input -> Int
part1 = solution 2

part2 :: Input -> Int
part2 = solution 10

main :: IO ()
main = do
	inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "sample2.txt", "input.txt"]
	traverse_ (print . part1) inputs
	putStrLn ""
	traverse_ (print . part2) inputs
