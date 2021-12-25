#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Bifunctor  (first, second)
import Data.Foldable   (traverse_)
import Data.List       qualified as L
import Data.Map.Strict qualified as M

enumerate :: (Enum i, Num i) => [a] -> [(i, a)]
enumerate = zip [0..]

data SeaCucumber = East | South deriving (Eq, Show)

type Point = (Word, Word)
type CucumberMap = M.Map Point SeaCucumber
type Input = (CucumberMap, Word, Word)

parse :: String -> Input
parse s =
  ( M.fromList $ enumerate ls >>= (\(y, l) -> [((y, x), c) | (x, Just c) <- enumerate $ f <$> l])
  , L.genericLength ls - 1
  , L.genericLength (head ls) - 1
  )
  where
    ls = lines s
    f '>' = Just East
    f 'v' = Just South
    f '.' = Nothing
    f  c  = error $ "invalid sea cucumber: " <> [c]

step' :: SeaCucumber -> ((Word -> Word) -> Point -> Point) -> Word -> CucumberMap -> CucumberMap
step' c g s m = M.foldrWithKey f m $ M.filter (==c) m
  where
    f p _ m' = maybe (M.insert p' c $ M.delete p m') (const m') $ m M.!? p'
      where p' = g (\a -> if a == s then 0 else a + 1) p

step :: Word -> Word -> CucumberMap -> CucumberMap
step sy sx = stepSouth . stepEast
  where
    stepEast  = step' East  second sx
    stepSouth = step' South first  sy

part1 :: Input -> Word
part1 (m, sy, sx) = go 1 m
  where
    go i m' = if m' == m'' then i else go (i + 1) m''
      where
        m'' = step sy sx m'

-- part2 :: Input -> Word
-- part2 x = 0

main :: IO ()
main = do
  inputs <- traverse (fmap parse . readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  -- putStrLn ""
  -- traverse_ (print . part2) inputs
