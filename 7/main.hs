#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as T

solution :: (Int -> Int) -> [Int] -> Int
solution calculateFuel crabs = minimum
   $  (\position -> sum $ calculateFuel . abs . (position -) <$> crabs)
  <$> [minimum crabs..maximum crabs]

main :: IO ()
main = do
  input <- map (read . T.unpack) . T.splitOn "," <$> T.readFile "input.txt"
  -- Part 1
  print $ solution id input
  -- Part 2
  print $ solution (\x -> x * (x + 1) `div` 2) input
