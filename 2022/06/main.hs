#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -Wno-tabs #-}

import Data.Foldable (traverse_)
import Data.List     (dropWhileEnd, find)
import Data.Maybe    (fromJust)
import Data.Set      qualified as S

zipNList :: Int -> [a] -> [[a]]
zipNList _ [] = []
zipNList 0 _ = []
zipNList 1 xs = pure <$> xs
zipNList n xs@(_ : xs') = zipWith (:) xs (zipNList (n - 1) xs')

solution :: Int -> String -> Int
solution n = fst
           . fromJust
           . find (((==) <$> length <*> (S.size . S.fromList)) . snd)
           . zip [n..]
           . zipNList n

part1 :: String -> Int
part1 = solution 4

part2 :: String -> Int
part2 = solution 14

main :: IO ()
main = do
	input <- dropWhileEnd (== '\n') <$> readFile "input.txt"
	let inputs =
		[ input
		, "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
		, "bvwbjplbgvbhsrlpgdmjqwftvncz"
		, "nppdvjthqldpwncqszvftbrmjlhg"
		, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
		, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
		]
	traverse_ (print . part1) inputs
	putStrLn ""
	traverse_ (print . part2) inputs
