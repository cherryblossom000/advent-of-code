module Main

import Data.List1
import Data.String
import System
import System.File.ReadWrite

Range : Type
Range = (Nat, Nat)

data ParseError = InvalidNat String | NotATuple
Show ParseError where
	show (InvalidNat string) = "invalid natural number \{string}"
	show NotATuple           = "list does not have 2 elements"

toTuple : List1 a -> Either ParseError (a, a)
toTuple (a ::: [b]) = Right (a, b)
toTuple _           = Left NotATuple

splitOn : Char -> String -> List1 String
splitOn = split . (==)

Input : Type
Input = List (Range, Range)

parse : String -> Either ParseError (List (Range, Range))
parse = traverse (
          toTuple
      <=< traverse (
            toTuple
        <=< traverse (\s => maybe (Left $ InvalidNat s) Right $ parsePositive s)
          . splitOn '-'
          )
        . splitOn ','
        )
      . lines
      . trim

contains : Ord a => a -> (a, a) -> Bool
contains x (a, b) = a <= x && x <= b

rangeContains : Ord a => (Bool -> Lazy Bool -> Bool) -> (a, a) -> (a, a) -> Bool
rangeContains f (x, y) a = contains x a `f` contains y a

solution : (Bool -> Lazy Bool -> Bool) -> Input -> Nat
solution f = length . filter (\(a, b) => rangeContains f a b || rangeContains f b a)

part1 : Input -> Nat
part1 = solution (&&)

part2 : Input -> Nat
part2 = solution (||)

handleError : Show e => e -> IO ()
handleError = (*> exitFailure) . printLn

main : IO ()
main = do
	Right input <- readFile {io = IO} "input.txt"
	| Left e => handleError e
	let Right xs = parse input
	| Left e => handleError e
	printLn $ part1 xs
	printLn $ part2 xs
