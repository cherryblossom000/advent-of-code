#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wno-tabs #-}

import Data.Foldable (foldl', traverse_)
import Data.List     (sortBy)
import Data.Text     qualified as T
import Data.Text.IO  qualified as T

readInt :: T.Text -> Int
readInt = read . T.unpack

type MonkeyNo = Int
type WorryLevel = Int
data Test = Test { divisibleBy :: WorryLevel
                 , true :: MonkeyNo
                 , false :: MonkeyNo
                 }
data Monkey = Monkey { items :: [WorryLevel]
                     , operation :: WorryLevel -> WorryLevel
                     , test :: Test
                     , inspectCount :: Int
                     }

parseOperation :: T.Text -> WorryLevel -> WorryLevel
parseOperation t old =
	let
		[a, b, c] = T.words t
		parseTerm "old" = old
		parseTerm x     = readInt x
	in (if b == "+" then (+) else (*)) (parseTerm a) (parseTerm c)

parseMonkey :: T.Text -> Monkey
parseMonkey t =
	let
		[_, start, op, cond, true, false] = T.lines t
		drop = T.drop . T.length
	in Monkey
		(readInt <$> T.splitOn "," (drop "  Starting items: " start))
		(parseOperation $ drop "  Operation: new = " op)
		(Test (readInt $ drop "  Test: divisible by " cond)
		      (readInt $ drop "    If true: throw to monkey " true)
		      (readInt $ drop "    If false: throw to monkey " false))
		0

parse :: T.Text -> [Monkey]
parse = map parseMonkey . T.splitOn "\n\n"

modifyAt :: (a -> a) -> Int -> [a] -> [a]
modifyAt f i xs =
	let (a, b : cs) = splitAt i xs
	in a <> (f b : cs)

nTimes :: Int -> (a -> a) -> a -> a
nTimes n = foldr (.) id . replicate n

inspectItem :: (WorryLevel -> WorryLevel) -> Monkey -> [Monkey] -> WorryLevel -> [Monkey]
inspectItem fn (Monkey _ op (Test d t f) _) ms item =
	let new = fn (op item)
	in modifyAt (\m -> m { items = new : items m }) (if new `mod` d == 0 then t else f) ms

runTurn :: (WorryLevel -> WorryLevel) -> [Monkey] -> MonkeyNo -> [Monkey]
runTurn f ms i =
	let m@(Monkey items _ _ _) = ms !! i
	in modifyAt (\m' -> m' { items = [], inspectCount = inspectCount m' + length items }) i
	 $ foldl' (inspectItem f m) ms items

runRound :: (WorryLevel -> WorryLevel) -> [Monkey] -> [Monkey]
runRound f ms = foldl' (runTurn f) ms $ [0..(length ms - 1)]

solution :: Int -> (WorryLevel -> WorryLevel) -> [Monkey] -> Int
solution n f ms = product
                . take 2
                . sortBy (flip compare)
                $ inspectCount
               <$> nTimes n (runRound f) ms

part1 :: [Monkey] -> Int
part1 = solution 20 (`div` 3)

part2 :: [Monkey] -> Int
part2 ms = solution 10_000 (`mod` product (divisibleBy . test <$> ms)) ms

main :: IO ()
main = do
	inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "input.txt"]
	traverse_ (print . part1) inputs
	putStrLn ""
	traverse_ (print . part2) inputs
