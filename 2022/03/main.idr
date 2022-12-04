module Main

import Data.Nat
import Data.String
import System
import System.File.ReadWrite

partial fromJust : Maybe a -> a
fromJust (Just a) = a

parse : String -> List String
parse = lines . trim

priority : Char -> Nat
priority c = if n >= 97 then n `minus` 96 else n `minus` 38
	where
		n : Nat
		n = cast c

partial part1 : List String -> Nat
part1 = sum . map (\l =>
	let (as, bs) = splitAt (length l `divNat` 2) $ fastUnpack l
	in priority . fromJust $ find (`elem` bs) as)

chunk : Nat -> List a -> List (List a)
chunk n = unfoldr $ (\x => if null (fst x) then Nothing else Just x) . splitAt n

partial part2 : List String -> Nat
part2 = sum
      . map (\[as, bs, cs] =>
          priority
        . fromJust
        . find (\x => (x `elem` (fastUnpack bs)) && (x `elem` (fastUnpack cs)))
        $ fastUnpack as
      )
      . chunk 3

partial main : IO ()
main = do
	Right input <- readFile {io = IO} "input.txt"
	| Left e => die (show e)
	let xs = parse input
	printLn $ part1 xs
	printLn $ part2 xs
