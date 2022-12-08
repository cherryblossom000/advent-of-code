module Main

import Data.String
import Data.Vect
import System
import System.File.ReadWrite

-- yay this was so nice when I could just write it like this and it WORKED
-- no weird proof shenanigans needed
zipNList : (n : Nat) -> List a -> List (Vect n a)
zipNList _       []            = []
zipNList 0       _             = []
zipNList 1       xs            = pure <$> xs
zipNList n@(S m) xs@(_ :: xs') = zipWith (::) xs $ zipNList m xs'

parse : String -> List Char
parse = fastUnpack . trim

solution : Nat -> List Char -> Maybe Nat
solution n = map fst
           . find (\(_, xs) => length xs == length (snd $ nub xs))
           -- sometimes I want laziness back
           -- zip [n..]
           . (\xs => zip (toList $ Stream.take (length xs) [n..]) xs)
           . zipNList n

part1 : List Char -> Maybe Nat
part1 = solution 4

part2 : List Char -> Maybe Nat
part2 = solution 14

main : IO ()
main = do
	Right input <- map parse <$> readFile {io = IO} "input.txt"
		| Left e => die (show e)
	printLn $ part1 input
	printLn $ part2 input
