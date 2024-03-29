module Main

import Data.String
import System
import System.File.ReadWrite

Shape : Type
Shape = Int

Outcome : Type
Outcome = Int

Input : Type
Input = List (Shape, Int)

parse : String -> Maybe Input
parse = traverse ((\l => Just (ord !(getAt 0 l) - 65, ord !(getAt 2 l) - 88)) . fastUnpack)
      . lines
      . trim

shapeScore : Shape -> Int
shapeScore = (+1)

outcomeScore : Outcome -> Int
outcomeScore = (*3)

part1 : Input -> Int
part1 = sum . map (\(a, b) => shapeScore b + outcomeScore ((b - a + 1) `mod` 3))

part2 : Input -> Int
part2 = sum . map (\(a, b) => shapeScore ((a + b - 1) `mod` 3) + outcomeScore b)

main : IO ()
main = do
	Right input <- readFile {io = IO} "input.txt"
	| Left e => die (show e)
	let Just xs = parse input
	| Nothing => die "parse error"
	printLn $ part1 xs
	printLn $ part2 xs
