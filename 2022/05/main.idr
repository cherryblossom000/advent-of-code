module Main

import Data.String
import Data.Vect
import System
import System.File.ReadWrite

splitOn2NewLines : String -> List (List String)
splitOn2NewLines string = go [] $ lines string
	where
		go : List String -> List String -> List (List String)
		go acc [] = [acc]
		go acc ("" :: xs) = acc :: go [] xs
		go acc (x :: xs) = go (acc ++ [x]) xs

Crate : Type
Crate = Char

Stacks : Nat -> Type
Stacks n = Vect n (List Crate)

record Step n where
	constructor MkStep
	number : Nat
	from, to : Fin n

Input : Nat -> Type
Input n = (Stacks n, List (Step n))

chunk : Nat -> List a -> List (List a)
chunk n = unfoldr $ (\x => if null (fst x) then Nothing else Just x) . splitAt n

rtrim : String -> String
rtrim = reverse . ltrim . reverse

parse : String -> Maybe (n ** Input n)
parse s = do
	let [cratesLines'@(_ :: _), stepsLines] = splitOn2NewLines $ rtrim s
	| _ => Nothing
	cratesLines <- traverse (traverse (getAt 1) . chunk 4 . fastUnpack) $ init cratesLines'
	let crates = fromList $ dropWhile (== ' ') <$> transpose cratesLines
	steps <- traverse ((\case
			[_, a, _, b, _, c] =>
				let parseIndex = (\m => natToFin (m `minus` 1) _) <=< parsePositive
				in Just $ MkStep !(parsePositive a) !(parseIndex b) !(parseIndex c)
			_ => Nothing
		) . words) stepsLines
	Just (_ ** (crates, steps))

solution' : (forall m. Step m -> Stacks m -> Stacks m) -> Input n -> String
solution' runStep (stacks, steps) = fastPack $ (maybe [] pure . head') =<< toList (foldl (flip runStep) stacks steps)

solution : (List Crate -> List Crate) -> Input n -> String
solution f = solution' $ \step, crates =>
               updateAt step.to (f (take step.number $ index step.from crates) ++)
             $ updateAt step.from (drop step.number) crates

part1 : Input n -> String
part1 = solution reverse

part2 : Input n -> String
part2 = solution id

main : IO ()
main = do
	Right input <- readFile {io = IO} "input.txt"
	| Left e => die (show e)
	let Just (_ ** xs) = parse input
	| Nothing => die "parsing error"
	putStrLn $ part1 xs
	putStrLn $ part2 xs
