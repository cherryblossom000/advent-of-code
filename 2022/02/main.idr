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

partial parse : String -> Input
parse = map (\l => (ord (l `strIndex` 0) - 65, ord (l `strIndex` 2) - 88)) . lines . trim

shapeScore : Shape -> Int
shapeScore = (+1)

outcomeScore : Outcome -> Int
outcomeScore = (*3)

part1 : Input -> Int
part1 = sum . map (\(a, b) => shapeScore b + outcomeScore ((b - a + 1) `mod` 3))

part2 : Input -> Int
part2 = sum . map (\(a, b) => shapeScore ((a + b - 1) `mod` 3) + outcomeScore b)

handleError : Show e => e -> IO ()
handleError = (*> exitFailure) . printLn

partial main : IO ()
main = do
	Right input <- readFile {io = IO} "input.txt"
	| Left e => handleError e
	let xs = parse input
	printLn $ part1 xs
	printLn $ part2 xs
