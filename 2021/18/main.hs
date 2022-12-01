#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -Wall #-}

import Control.Applicative          ((<|>))
import Data.Bifunctor               (second)
import Data.Foldable                (traverse_)
import Data.List                    (foldl1', tails)
import Data.Maybe                   (fromMaybe)
import Text.ParserCombinators.ReadP (ReadP, char, eof, readP_to_S)
import Text.Read.Lex                (readDecP)

runReadP :: Show a => ReadP a -> String -> a
runReadP r s = case readP_to_S (r <* eof) s of
  [(a, _)] -> a
  [] -> error "runReadP: no parse"
  xs -> error $ "runReadP: multiple parses: " <> show xs

data RecPair a = N a | P (RecPair a) (RecPair a) deriving (Eq, Show)
data Dir = L | R deriving (Eq, Show)

leftmost :: (a -> a) -> RecPair a -> RecPair a
leftmost f (N n) = N $ f n
leftmost f (P l r) = P (leftmost f l) r

rightmost :: (a -> a) -> RecPair a -> RecPair a
rightmost f (N n) = N $ f n
rightmost f (P l r) = P l (rightmost f r)

leftAdj :: (a -> a) -> [Dir] -> RecPair a -> RecPair a
leftAdj f (R:is) (P l r)
  | all (==L) is = P (rightmost f l) r
  | otherwise    = P l (leftAdj f is r)
leftAdj f (L:is) (P l r) = P (leftAdj f is l) r
leftAdj _ _ x = x

rightAdj :: (a -> a) -> [Dir] -> RecPair a -> RecPair a
rightAdj f (L:is) (P l r)
  | all (==R) is = P l (leftmost f r)
  | otherwise    = P (rightAdj f is l) r
rightAdj f (R:is) (P l r) = P l (rightAdj f is r)
rightAdj _ _ x = x

type Snailfish = RecPair Word

parseSnailfish :: ReadP Snailfish
parseSnailfish = N <$> readDecP <|> p
  where
    p = do
      _ <- char '['
      l <- parseSnailfish
      _ <- char ','
      r <- parseSnailfish
      _ <- char ']'
      pure $ P l r

parse :: String -> [Snailfish]
parse = map (runReadP parseSnailfish) . lines

findNested :: Snailfish -> Maybe (([Dir], (Word, Word)), Snailfish)
findNested = go (0 :: Word) []
  where
    go _ _ (N _) = Nothing
    go i is (P (N a) (N b)) | i >= 4 = Just ((is, (a, b)), N 0)
    go i is (P l r) =
      let i' = i + 1
      in second (`P` r) <$> go i' (is <> [L]) l <|> second (P l) <$> go i' (is <> [R]) r

explode :: Snailfish -> Maybe Snailfish
explode = fmap (\((is, (a, b)), x') -> rightAdj (+b) is $ leftAdj (+a) is x') . findNested

split :: Snailfish -> Maybe Snailfish
split (N n)
  | n >= 10   = let a = n `div` 2 in Just $ P (N a) (N $ if odd n then a + 1 else a)
  | otherwise = Nothing
split (P l r) = (`P` r) <$> split l <|> P l <$> split r

reduce :: Snailfish -> Snailfish
reduce x@(N _) = fromMaybe x $ split x
reduce x@(P _ _) = maybe x reduce $ explode x <|> split x

add :: Snailfish -> Snailfish -> Snailfish
add l r = reduce $ P l r

magnitude :: Snailfish -> Word
magnitude (N n) = n
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r

part1 :: [Snailfish] -> Word
part1 = magnitude . foldl1' add

part2 :: [Snailfish] -> Word
part2 xs = maximum [magnitude z | (x:xs') <- tails xs, y <- xs', z <- [add x y, add y x]]

main :: IO ()
main = do
  inputs <- traverse (fmap parse . readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
