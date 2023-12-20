#!/usr/bin/env runhaskell

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow ((&&&), (***))
import Data.Bifunctor (first, second)
import Data.Foldable (traverse_)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as T

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple _ = error "expected 2 elements"

type WorkflowName = T.Text
type Category = Char

data Rel = Lt | Gt
data Condition = Always | Category Category Rel Int

data RuleDest = Workflow WorkflowName | Accepted | Rejected
type Rule = (Condition, RuleDest)
type Workflow = [Rule]

type Part = M.Map Char Int
type Workflows = M.Map WorkflowName Workflow

matches :: Part -> Condition -> Bool
matches _ Always = True
matches p (Category c Lt n) = p M.! c < n
matches p (Category c Gt n) = p M.! c > n

type Input = (Workflows, [Part])

readInt :: T.Text -> Int
readInt = read . T.unpack

parseRuleDest :: T.Text -> RuleDest
parseRuleDest "A" = Accepted
parseRuleDest "R" = Rejected
parseRuleDest t = Workflow t

parseRule :: T.Text -> Rule
parseRule t = case T.splitOn ":" t of
  [cond, dest] ->
    ( Category
        (T.head cond)
        (if T.head (T.tail cond) == '>' then Gt else Lt)
        (readInt $ T.drop 2 cond)
    , parseRuleDest dest
    )
  [dest] -> (Always, parseRuleDest dest)
  _ -> error "invalid rule"

parseWorkflow :: T.Text -> (WorkflowName, Workflow)
parseWorkflow t = (name, parseRule <$> T.splitOn "," rules)
 where
  (name, rules) = toTuple . T.splitOn "{" $ T.init t

parsePart :: T.Text -> Part
parsePart =
  M.fromList
    . map (T.head &&& readInt . T.tail . T.tail)
    . T.splitOn ","
    . T.init
    . T.tail

parse :: T.Text -> Input
parse t =
  M.fromList . map parseWorkflow . T.lines *** map parsePart . T.lines $
    toTuple $
      T.splitOn "\n\n" t

isAccepted :: Workflows -> Workflow -> Part -> Bool
isAccepted ws rs p = case snd . fromJust $ L.find (matches p . fst) rs of
  Accepted -> True
  Rejected -> False
  Workflow w -> isAccepted ws (ws M.! w) p

part1 :: Input -> Int
part1 (ws, parts) = sum $ sum <$> filter (isAccepted ws (ws M.! "in")) parts

part2 :: Input -> Int
part2 (ws, _) = go (ws M.! "in") (M.fromList $ (,(1, 4000)) <$> "xmas")
 where
  go [] _ = 0
  go _ p | any (uncurry (>)) p = 0
  go ((c, d) : rs) p =
    go rs b + case d of
      Accepted | any (uncurry (>)) a -> 0
      Accepted -> product $ (+ 1) . uncurry (flip (-)) <$> a
      Rejected -> 0
      Workflow w -> go (ws M.! w) a
   where
    (a, b) = splitParts c p
  splitParts Always p = (p, M.fromList $ (,(1, 0)) <$> "xmas")
  splitParts (Category c Lt n) p =
    ( M.update (Just . second (min (n - 1))) c p
    , M.update (Just . first (max n)) c p
    )
  splitParts (Category c Gt n) p =
    ( M.update (Just . first (max (n + 1))) c p
    , M.update (Just . second (min n)) c p
    )

main :: IO ()
main = do
  inputs <- traverse (fmap parse . T.readFile) ["sample.txt", "input.txt"]
  traverse_ (print . part1) inputs
  putStrLn ""
  traverse_ (print . part2) inputs
