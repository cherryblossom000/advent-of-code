#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Map      qualified as M
import Data.Foldable (traverse_)
import Data.List     qualified as L

parse :: String -> (Int, String)
parse = parse' []
  where
    parse' stack [] = (0, stack)
    parse' ('(':stack) (')':cs) = parse' stack cs
    parse' ('[':stack) (']':cs) = parse' stack cs
    parse' ('{':stack) ('}':cs) = parse' stack cs
    parse' ('<':stack) ('>':cs) = parse' stack cs
    parse' stack (')':_) = (3, stack)
    parse' stack (']':_) = (57, stack)
    parse' stack ('}':_) = (1197, stack)
    parse' stack ('>':_) = (25137, stack)
    parse' stack (c:cs) = parse' (c:stack) cs

part1 :: [String] -> Int
part1 = sum . map (fst . parse)

part2PointsMap :: M.Map Char Int
part2PointsMap = M.fromList
  [ ('(', 1)
  , ('[', 2)
  , ('{', 3)
  , ('<', 4)
  ]

part2Points :: String -> Int
part2Points = L.foldl' (\acc c -> acc * 5 + part2PointsMap M.! c) 0

part2 :: [String] -> Int
part2 s = xs !! (length xs `div` 2)
  where xs = L.sort $ part2Points . snd <$> filter ((== 0) . fst) (parse <$> s)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputs = [sample, lines input]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs

sample :: [String]
sample =
  [ "[({(<(())[]>[[{[]{<()<>>"
  , "[(()[<>])]({[<{<<[]>>("
  , "{([(<{}[<>[]}>{[]{[(<()>"
  , "(((({<>}<{<{<>}{[]{[]{}"
  , "[[<[([]))<([[{}[[()]]]"
  , "[{[{({}]{}}([{[{{{}}([]"
  , "{<[[]]>}<{[{[{[]{()[[[]"
  , "[<(<(<(<{}))><([]([]()"
  , "<{([([[(<>()){}]>(<<{{"
  , "<{([{{}}[<[[[<>{}]]]>[]]"
  ]
