#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -Wall #-}

import Data.Foldable (traverse_)
import Data.List     (foldl', sort)

parse :: String -> (Word, String)
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

part1 :: [String] -> Word
part1 = sum . map (fst . parse)

part2Points :: String -> Word
part2Points = foldl' (\acc c -> acc * 5 + f c) 0
  where
    f '(' = 1
    f '[' = 2
    f '{' = 3
    f '<' = 4
    f  c  = error $ "invalid character: " <> [c]

part2 :: [String] -> Word
part2 s = xs !! (length xs `div` 2)
  where xs = sort $ part2Points . snd <$> filter ((== 0) . fst) (parse <$> s)

main :: IO ()
main = do
  inputs <- traverse (fmap lines . readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
