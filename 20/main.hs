#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Array      qualified as A
import Data.Bits       (Bits, (.|.), shiftL)
import Data.Bool       (bool)
import Data.Foldable   (foldl', traverse_)
import Data.Maybe      (fromMaybe)
import Data.Map.Strict qualified as M
import Data.Word       (Word16)

enumerate :: (Enum i, Num i) => [a] -> [(i, a)]
enumerate = zip [0..]

nTimes :: Word -> (a -> a) -> a -> a
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

type Pixel = Bool
type Algorithm = A.Array Word16 Pixel
type Point = (Int, Int)
type Image = (M.Map Point Pixel, Pixel)
type Input = (Algorithm, Image)

parse :: String -> Input
parse s =
  ( A.listArray (0, 511) $ f <$> alg
  , (M.fromList $ [((i, j), f p) | (i, ps) <- enumerate img, (j, p) <- enumerate ps], False)
  )
  where
    alg:_:img = lines s
    f = (=='#')

pixelsToInt :: (Bits i, Num i) => [Pixel] -> i
pixelsToInt = foldl' (\acc x -> acc `shiftL` 1 .|. bool 0 1 x) 0

square :: Point -> [Point]
square (i, j) =
  [ (i - 1, j - 1), (i - 1, j), (i - 1, j + 1)
  , (i    , j - 1), (i    , j), (i    , j + 1)
  , (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)
  ]

enhance :: Algorithm -> Image -> Image
enhance alg (img, bg) =
  ( M.fromList
    [ (p, alg A.! pixelsToInt (fromMaybe bg . (img M.!?) <$> square p))
    | i <- [minX-2 .. maxX+2]
    , j <- [minY-2 .. maxY+2]
    , let p = (i, j)
    ]
  , alg A.! 0 && not bg
  )
  where
    ((minY, minX), _) = M.findMin img
    ((maxY, maxX), _) = M.findMax img

solution :: Word -> Input -> Int
solution n (alg, img) = M.size . M.filter id . fst $ nTimes n (enhance alg) img

part1 :: Input -> Int
part1 = solution 2

part2 :: Input -> Int
part2 = solution 50

main :: IO ()
main = do
  inputs <- traverse (fmap parse . readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
