module Main

import Control.Monad.State
import Data.SortedMap
import Data.String
import Data.Vect
import System
import System.File.ReadWrite

record Directory where
	constructor MkDirectory
	unDirectory : SortedMap String (Either Nat Directory)

Show Directory where
	show (MkDirectory m) = "MkDirectory \{assert_total show m}"

pretty : Directory -> String
pretty (MkDirectory m) = "- / (dir) \{unlines $ indent 2 <$> go m}"
	where
	go : SortedMap String (Either Nat Directory) -> List String
	go = (\(k, v) => case v of
			Left s => ["- \{k} (file, size=\{show s})"]
			Right (MkDirectory m) => "- \{k} (dir)" :: (indent 2 <$> go m)
		) <=< SortedMap.toList

empty : Directory
empty = MkDirectory empty

parse : String -> Maybe Directory
parse = map fst . foldlM f (empty, []) . map fastUnpack . lines
	where
	cd : List Char -> List String -> Maybe (List String)
	cd ['.','.'] []        = Nothing
	cd ['.','.'] (_ :: xs) = Just xs
	cd ['/']     _         = Just []
	cd dir       cwd       = Just $ fastPack dir :: cwd

	insertFile : List String -> String -> Nat -> Directory -> Maybe Directory
	insertFile []        name size (MkDirectory m) = Just . MkDirectory $ insert name (Left size) m
	insertFile (x :: xs) name size (MkDirectory m) = do
		m' <- case SortedMap.lookup x m of
			Just (Left _) => Nothing
			Just (Right dir) => Just dir
			Nothing => Just empty
		Just $ MkDirectory . insert x (Right !(insertFile xs name size m')) $ m

	f : (Directory, List String) -> List Char -> Maybe (Directory, List String)
	f (m, cwd) ('$'::' '::'c'::'d'::' '::dir) = Just (m, !(cd dir cwd))
	f acc ['$',' ','l','s'] = Just acc
	f (m, cwd) line =
		let (size, name) = mapFst fastPack $ break (== ' ') line
		in (, cwd) <$> if size == "dir"
			then Just m
			else insertFile (reverse cwd) (fastPack !(tail' name)) !(parsePositive size) m

directorySizes : Directory -> List Nat
directorySizes = execState [] . go 0
	where
		go : Nat -> Directory -> State (List Nat) Nat
		go n dir@(MkDirectory m) = do
			let Just (k, v) = leftMost m
				| Nothing => n <$ modify (n ::)
			let dir' = MkDirectory $ delete k m
			case v of
				Left size => go (n + size) dir'
				Right d => go (n + !(go 0 d)) dir'

part1 : Directory -> Nat
part1 = sum . filter (<= 100_000) . directorySizes

part2 : Directory -> Maybe Int
part2 root =
	-- not sure how to get Idris to optimise the Nats
	let sizes@(used :: _) = cast {to=Int} <$> directorySizes root
		| [] => Nothing
	in find (>= used - 40_000_000) $ sort sizes

main : IO ()
main = do
	Right rawInputs <- sequence <$> traverse readFile (the (Vect _ _) ["sample.txt", "input.txt"])
		| Left e => die (show e)
	let Just inputs = the (Maybe _) $ traverse parse rawInputs
		| Nothing => die "parse error"
	traverse_ (printLn . part1) inputs
	traverse_ (printLn . part2) inputs
