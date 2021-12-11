#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad       ((<=<), replicateM, when)
import Control.Monad.State (State, evalState, get, gets, modify)
import Data.Foldable       (traverse_)
import Data.Map.Strict     qualified as M

type Point = (Int, Int)
type Grid = M.Map Point Int

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

parse :: String -> Grid
parse = M.fromList .
  (   (\(i, xs) -> (\(j, x) -> ((i, j), read [x])) <$> enumerate xs)
  <=< (enumerate . lines)
  )

neighbours :: Point -> [Point]
neighbours (i, j) =
  [ (i - 1, j - 1), (i - 1, j), (i - 1, j + 1)
  , (i    , j - 1),             (i    , j + 1)
  , (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)
  ]

type S = State Grid

flash :: Point -> S ()
flash = traverse_ f . neighbours
  where
    f n = do
      modify $ M.adjust (+1) n
      x <- gets (M.!? n)
      when (x == Just 10) (flash n)

step :: S Int
step = do
  modify $ fmap (+1)
  _ <- get >>= M.traverseWithKey (const . flash) . M.filter (==10)
  modify $ fmap \x -> if x > 9 then 0 else x
  gets $ M.size . M.filter (==0)

part1 :: Grid -> Int
part1 = sum . evalState (replicateM 100 step)

part2 :: Grid -> Int
part2 = evalState $ go 1
  where
    go i = do
      n <- step
      s <- gets M.size
      if n == s then pure i else go (i + 1)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputs = parse <$> [sample, input]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs

sample :: String
sample = unlines
  [ "5483143223"
  , "2745854711"
  , "5264556173"
  , "6141336146"
  , "6357385478"
  , "4167524645"
  , "2176841721"
  , "6882881134"
  , "4846848554"
  , "5283751526"
  ]
