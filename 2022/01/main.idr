module Main

import Data.List1
import Data.String
import System
import System.File.ReadWrite

splitOn2NewLines : String -> List (List String)
splitOn2NewLines string = go [] $ lines string
	where
		go : List String -> List String -> List (List String)
		go acc [] = [acc]
		go acc ("" :: xs) = acc :: go [] xs
		go acc (x :: xs) = go (acc ++ [x]) xs

data ParseError = InvalidNat String | Empty
Show ParseError where
	show (InvalidNat string) = "invalid natural number \{string}"
	show Empty = "empty input"

parseAndSort : String -> Either ParseError (List1 Nat)
parseAndSort = maybe (Left Empty) Right . toList1' . sortBy (flip compare)
           <=< traverse (map sum . traverse (\s => maybe (Left $ InvalidNat s) Right $ parsePositive s))
             . splitOn2NewLines

part1 : List1 Nat -> Nat
part1 = head

part2 : List1 Nat -> Nat
part2 = sum . take 3 . forget

die' : Show e => e -> IO ()
die' = die . show

main : IO ()
main = do
	Right input <- readFile {io = IO} "input.txt"
		| Left e => die' e
	let Right xs = parseAndSort input
		| Left e => die' e
	printLn $ part1 xs
	printLn $ part2 xs
