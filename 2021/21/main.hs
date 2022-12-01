#!/usr/bin/env runhaskell

{-# LANGUAGE BlockArguments, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow    ((&&&))
import Control.Monad    (join)
import Data.Bifunctor   (bimap, first)
import Data.Bifoldable  (bimaximum)
import Data.Foldable    (traverse_)
import Data.IORef       (newIORef, modifyIORef, readIORef)
import Data.List        qualified as L
import Data.Map.Strict  qualified as M
import Data.Monoid      (Sum(Sum), getSum)
import System.IO.Unsafe (unsafePerformIO)

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _      = error "expected 2 elements"

foldMapWithKey' :: Monoid m => (k -> a -> m) -> M.Map k a -> m
foldMapWithKey' f = M.foldlWithKey' (\acc k a -> acc <> f k a) mempty

infixl 7 `mod'`
mod' :: Integral a => a -> a -> a
mod' a b = let n = a `mod` b in if n == 0 then b else n

data Player = Player { space, score :: Word } deriving (Show, Eq, Ord)
type Input = (Player, Player)

parse :: String -> Input
parse = toTuple
      . map ((`Player` 0) . read . drop (length "Player 1 starting position: "))
      . lines

data Game = Game Player Player Bool deriving (Eq, Ord, Show)

turn :: Game -> Word -> Game
turn g d = case g of
  (Game p1 p2 False) -> Game (f p1) p2 True
  (Game p1 p2 True ) -> Game p1 (f p2) False
  where f (Player sp sc) = let sp' = (sp + d) `mod'` 10 in Player sp' (sc + sp')

play1 :: (Game, Word) -> (Game, Word)
play1 (g, i) = (turn g . sum $ (`mod'` 100) <$> [i..i+2], i + 3)

part1 :: Input -> Word
part1 (p1, p2) = go (Game p1 p2 False, 1)
  where
    go g@(Game (Player _ p1sc) (Player _ p2sc) _, i)
      | p1sc >= 1000 = p2sc * (i - 1)
      | p2sc >= 1000 = p1sc * (i - 1)
      | otherwise    = go $ play1 g

diracDice :: [(Word, Word)]
diracDice = (head &&& L.genericLength) <$>
  L.group (L.sort [a + b + c | a <- [1, 2, 3], b <- [1, 2, 3], c <- [1, 2, 3]])

play2 :: Game -> M.Map Game Word
play2 g = M.fromList $ first (turn g) <$> diracDice

-- TODO: find better way without unsafePerformIO
cache :: Ord a => (a -> b) -> a -> b
cache f = unsafePerformIO do
  r <- newIORef M.empty
  pure \x -> unsafePerformIO do
    c <- readIORef r
    maybe (let y = f x in y <$ modifyIORef r (M.insert x y)) pure $ c M.!? x

part2 :: Input -> Word
part2 (p1, p2) = getSum . bimaximum . go $ Game p1 p2 False
  where
    go = cache go'
    go' g@(Game (Player _ p1sc) (Player _ p2sc) _)
      | p1sc >= 21 = (1, 0)
      | p2sc >= 21 = (0, 1)
      | otherwise  = foldMapWithKey' (\g' c -> join bimap (* Sum c) (go g')) $ play2 g

main :: IO ()
main = do
  inputs <- traverse (fmap parse . readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
