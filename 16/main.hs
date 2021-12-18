#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -Wall #-}

import Data.Bits     ((.|.), shiftL)
import Data.Bool     (bool)
import Data.Foldable (foldl', traverse_)
import Text.ParserCombinators.ReadP
  ( ReadP
  , count
  , eof
  , gather
  , get
  , munch
  , readP_to_S
  )
import Text.Read.Lex (readIntP)

boolToInt :: Integral a => Bool -> a
boolToInt = bool 0 1

runReadP :: Show a => ReadP a -> String -> a
runReadP r s = case readP_to_S (r <* eof) s of
  [(a, _)] -> a
  [] -> error "runReadP: no parse"
  xs -> error $ "runReadP: multiple parses: " <> show xs

readBin :: (Eq a, Num a, Show a) => String -> a
readBin = runReadP $ readIntP 2 ((||) <$> (=='0') <*> (=='1')) (bool 1 0 . (=='0'))

hexToBin :: Char -> String
-- Thanks Copilot
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin  c  = error $ "hexToBin: invalid hex char: " <> [c]

data OperatorType = Sum | Prod | Min | Max | Gt | Lt | Eq deriving Show

parseOpType :: Word -> OperatorType
parseOpType 0 = Sum
parseOpType 1 = Prod
parseOpType 2 = Min
parseOpType 3 = Max
parseOpType 5 = Gt
parseOpType 6 = Lt
parseOpType 7 = Eq
parseOpType n = error $ "parseOpType: invalid op type: " <> show n

data PacketData = Lit Word | Op OperatorType [Packet] deriving Show
data Packet = Packet Word PacketData deriving Show

parseLit :: ReadP Word
parseLit = foldl' (\a x -> a `shiftL` 4 .|. x) 0 <$> go
  where
    go = do
      x <- get
      y <- readBin <$> count 4 get
      if x == '0' then pure [y] else (y:) <$> go

parseOpBits :: ReadP [Packet]
parseOpBits = do
  l <- readBin <$> count 15 get
  let go i = if i == l
      then pure []
      else gather parsePacket >>= \(s, p) -> (p:) <$> go (i + length s)
  go 0

parseOpPackets :: ReadP [Packet]
parseOpPackets = count 11 get >>= (`count` parsePacket) . readBin

parseOp :: ReadP [Packet]
parseOp = get >>= bool parseOpPackets parseOpBits . (=='0')

parsePacket :: ReadP Packet
parsePacket = do
  v <- readBin <$> count 3 get
  t <- readBin <$> count 3 get
  Packet v <$> if t == 4 then Lit <$> parseLit else Op (parseOpType t) <$> parseOp

parse :: String -> Packet
parse = runReadP (parsePacket <* munch (=='0')) . (>>= hexToBin)

part1 :: Packet -> Word
part1 (Packet v (Lit   _)) = v
part1 (Packet v (Op _ ps)) = v + sum (part1 <$> ps)

part2 :: Packet -> Word
part2 (Packet _ (Lit        x)) = x
part2 (Packet _ (Op Sum    xs)) = sum       $ part2 <$> xs
part2 (Packet _ (Op Prod   xs)) = product   $ part2 <$> xs
part2 (Packet _ (Op Min    xs)) = minimum   $ part2 <$> xs
part2 (Packet _ (Op Max    xs)) = maximum   $ part2 <$> xs
part2 (Packet _ (Op Gt [x, y])) = boolToInt $ part2 x >  part2 y
part2 (Packet _ (Op Lt [x, y])) = boolToInt $ part2 x <  part2 y
part2 (Packet _ (Op Eq [x, y])) = boolToInt $ part2 x == part2 y
part2 _                         = error "invalid packet"

main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputs1 =
        [ "8A004A801A8002F478" -- 16
        , "620080001611562C8802118E34" -- 12
        , "C0015000016115A2E0802F182340" -- 23
        , "A0016C880162017C3686B18A3D4780" -- 31
        , input
        ]
  let inputs2 =
        [ "C200B40A82" -- 3
        , "04005AC33890" -- 54
        , "880086C3E88112" -- 7
        , "CE00C43D881120" -- 9
        , "D8005AC2A8F0" -- 1
        , "F600BC2D8F" -- 0
        , "9C005AC2F8F0" -- 0
        , "9C0141080250320F1802104A08" -- 1
        , input
        ]
  traverse_ (print . part1) $ parse <$> inputs1
  putStrLn ""
  traverse_ (print . part2) $ parse <$> inputs2
