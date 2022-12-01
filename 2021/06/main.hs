#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Data.IntMap.Strict qualified as M
import Data.Text          qualified as T
import Data.Text.IO       qualified as T

nTimes :: Word -> (a -> a) -> a -> a
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

solution :: Word -> [Int] -> Int
solution days = sum . nTimes days f . M.fromListWith (+) . map (,1)
  where
    f m = M.insert 8 fishAt0
        . M.adjust (+fishAt0) 6
        . foldr ($) m $ (\n -> M.insert (n - 1) (m ! n)) <$> [1..8]
      where fishAt0 = m ! 0
    (!) = flip $ M.findWithDefault 0

main :: IO ()
main = do
  input <- map (read . T.unpack) . T.splitOn "," <$> T.readFile "input.txt"
  -- Part 1
  print . solution  80 $ input
  -- Part 2
  print . solution 256 $ input
