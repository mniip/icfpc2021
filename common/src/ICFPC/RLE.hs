{-# LANGUAGE DerivingStrategies, BangPatterns #-}
module ICFPC.RLE where

import qualified Data.IntMap as IM

-- run length encoded set of ints
-- a pair (a, b) in the sequence encodes an interval [a, b)
-- b must be greater than a, but smaller than the next a in the list
newtype RLESeq = RLESeq [(Int, Int)]
  deriving stock (Eq, Ord, Show)

empty :: RLESeq
empty = RLESeq []

-- encode an interval [a, b)
run :: Int -> Int -> RLESeq
run a b
  | a < b = RLESeq [(a, b)]
  | otherwise = empty

singleton :: Int -> RLESeq
singleton a = RLESeq [(a, a + 1)]

toList :: RLESeq -> [Int]
toList (RLESeq is) = [x | (a, b) <- is, x <- [a..b-1]]

-- strictly ascending
fromAscList :: [Int] -> RLESeq
fromAscList [] = empty
fromAscList (x:xs) = RLESeq $ go x x xs
  where
    go !a !b [] = [(a, b + 1)]
    go !a !b (!c:xs)
      | c == b + 1 = go a (b + 1) xs
      | otherwise = (a, b + 1):go c c xs

union :: RLESeq -> RLESeq -> RLESeq
union (RLESeq is) (RLESeq js) = RLESeq $ fromFlips $ merge00 (toFlips is) (toFlips js)
  where
    toFlips = concatMap (\(a, b) -> [a, b])
    fromFlips [] = []
    fromFlips (a:b:es) = (a, b):fromFlips es
    fromFlips _ = error "fromFlips"
    merge00 [] js = js
    merge00 is [] = is
    merge00 (i:is) (j:js) = case compare i j of
      LT -> i:merge10 is (j:js)
      EQ -> i:merge11 is js
      GT -> j:merge01 (i:is) js
    merge01 [] js = js
    merge01 is [] = error "merge01"
    merge01 (i:is) (j:js) = case compare i j of
      LT -> merge11 is (j:js)
      EQ -> merge10 is js
      GT -> j:merge00 (i:is) js
    merge10 [] js = error "merge10"
    merge10 is [] = is
    merge10 (i:is) (j:js) = case compare i j of
      LT -> i:merge00 is (j:js)
      EQ -> merge01 is js
      GT -> merge11 (i:is) js
    merge11 [] js = error "merge11"
    merge11 is [] = error "merge11"
    merge11 (i:is) (j:js) = case compare i j of
      LT -> merge01 is (j:js)
      EQ -> i:merge00 is js
      GT -> merge10 (i:is) js

-- a tree version of the above sequence, for efficient member lookup
newtype RLESet = RLESet (IM.IntMap Int)
  deriving (Eq, Ord, Show)

fromSeq :: RLESeq -> RLESet
fromSeq (RLESeq is) = RLESet $ IM.fromList is

toSeq :: RLESet -> RLESeq
toSeq (RLESet m) = RLESeq $ IM.toList m

member :: Int -> RLESet -> Bool
member x (RLESet m) = case IM.lookupLE x m of
  Nothing -> False
  Just (_, b) -> x < b
