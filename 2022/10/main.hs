#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase, ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wno-tabs #-}

import Data.Foldable (traverse_)
import Data.List     (unfoldr)
import Data.Text     qualified as T
import Data.Text.IO  qualified as T

parse :: T.Text -> [Int]
parse = (>>= parseInstruction . T.words) . T.lines
	where
		parseInstruction ["noop"] = [0]
		parseInstruction ["addx", n] = [0, read (T.unpack n)]

run :: [Int] -> [Int]
run = scanl (+) 1

part1 :: [Int] -> Int
part1 xs = sum $ (\i -> i * run xs !! (i - 1)) <$> [20, 60, 100, 140, 180, 220]

chunks :: Int -> [a] -> [[a]]
chunks n = unfoldr $ \case
	[] -> Nothing
	xs -> Just $ splitAt n xs

part2 :: [Int] -> String
part2 = unlines . map (map f . zip [0..]) . chunks 40 . run
	where
		f (i, x) = if i - 1 <= x && x <= i + 1 then '#' else '.'

main :: IO ()
main = do
	inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "input.txt"]
	traverse_ (print . part1) inputs
	putStrLn ""
	traverse_ (putStrLn . part2) inputs
