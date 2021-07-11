{-# LANGUAGE DerivingStrategies, BangPatterns #-}
module ICFPC.RLE where

import qualified Data.IntMap as IM

-- run length encoded set of ints
-- a pair (a, b) in the sequence encodes an interval [a, b)
-- b must be greater than a, but smaller than the next a in the list
newtype RLE = RLE [(Int, Int)]
  deriving stock (Eq, Ord, Show)

empty :: RLE
empty = RLE []

-- encode an interval [a, b)
run :: Int -> Int -> RLE
run a b
  | a < b = RLE [(a, b)]
  | otherwise = empty

singleton :: Int -> RLE
singleton a = RLE [(a, a + 1)]

toList :: RLE -> [Int]
toList (RLE is) = [x | (a, b) <- is, x <- [a..b-1]]

toRuns :: RLE -> [(Int, Int)]
toRuns (RLE is) = is

-- strictly ascending
fromAscList :: [Int] -> RLE
fromAscList [] = empty
fromAscList (x:xs) = RLE $ go x x xs
  where
    go !a !b [] = [(a, b + 1)]
    go !a !b (!c:xs)
      | c == b + 1 = go a (b + 1) xs
      | otherwise = (a, b + 1):go c c xs

union :: RLE -> RLE -> RLE
union (RLE is) (RLE js) = RLE $ fromFlips $ merge00 (toFlips is) (toFlips js)
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

member :: Int -> RLE -> Bool
member x (RLE is) = go is
  where
    go [] = False
    go ((a, b):is)
      | a > x = False
      | x < b = True
      | otherwise = go is
