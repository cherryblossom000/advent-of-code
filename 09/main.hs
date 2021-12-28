#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad       ((<=<), unless)
import Control.Monad.State (State, execState, get, modify)
import Data.Foldable       (traverse_)
import Data.List           qualified as L
import Data.Map            qualified as M
import Data.Map            ((!), (!?))
import Data.Set            qualified as S

type Point = (Word, Word)
type Input = M.Map Point Word

enumerate :: (Enum i, Num i) => [a] -> [(i, a)]
enumerate = zip [0..]

parse :: String -> Input
parse = M.fromList .
  (   (\(i, xs) -> (\(j, x) -> ((i, j), read [x])) <$> enumerate xs)
  <=< (enumerate . lines)
  )

data Direction = U | D | L | R deriving (Eq, Show)

allDirs :: [Direction]
allDirs = [U, D, L, R]

dirFn :: Point -> Direction -> Point
dirFn (i, j) U = (i - 1, j)
dirFn (i, j) D = (i + 1, j)
dirFn (i, j) L = (i, j - 1)
dirFn (i, j) R = (i, j + 1)

rotL :: Direction -> Direction
rotL U = L
rotL R = U
rotL D = R
rotL L = D

rotR :: Direction -> Direction
rotR U = R
rotR R = D
rotR D = L
rotR L = U

lowPoints :: Input -> [Point]
lowPoints xs = M.foldMapWithKey
  (\p x -> [p | all (maybe True (>x) . (xs!?) . dirFn p) allDirs])
  xs

part1 :: Input -> Word
part1 xs = sum $ (+1) . (xs!) <$> lowPoints xs

type S = State (S.Set Point) ()

search :: Input -> Point -> Point -> Direction -> S
search xs p p' d = f $ xs !? p'
  where
    f (Just x) | x > xs ! p && x /= 9 = search' xs p' [d, rotL d, rotR d]
    f _ = pure ()

search' :: Input -> Point -> [Direction] -> S
search' xs p ds = modify (S.insert p) *> traverse_ f ds
  where
    f d = get >>= (`unless` search xs p p' d) . (p' `S.member`)
      where p' = p `dirFn` d

basinPoints :: Input -> Point -> Int
basinPoints xs p = S.size $ execState (search' xs p allDirs) S.empty

part2 :: Input -> Int
part2 xs = product . take 3 . L.sortBy (flip compare) $ basinPoints xs <$> lowPoints xs

main :: IO ()
main = do
  inputs <- traverse (fmap parse . readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
