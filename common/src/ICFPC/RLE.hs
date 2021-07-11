{-# LANGUAGE DerivingStrategies, BangPatterns #-}
module ICFPC.RLE where

import Data.List (foldl')

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
union (RLE is) (RLE js) = RLE $ merge00 is js
  where
    merge00 [] js = js
    merge00 is [] = is
    merge00 (i@(a, b):is) (j@(c, d):js) = case compare a c of
      LT -> merge10 a b is (j:js)
      EQ -> merge11 a b is d js
      GT -> merge01 c (i:is) d js
    merge01 p [] d js = (p, d):js
    merge01 p (i@(a, b):is) d js = case compare a d of
      LT -> merge11 p b is d js
      EQ -> merge10 p b is js
      GT -> (p, d):merge00 (i:is) js
    merge10 p b is [] = (p, b):is
    merge10 p b is (j@(c, d):js) = case compare b c of
      LT -> (p, b):merge00 is (j:js)
      EQ -> merge01 p is d js
      GT -> merge11 p b is d js
    merge11 p b is d js = case compare b d of
      LT -> merge01 p is d js
      EQ -> (p, b):merge00 is js
      GT -> merge10 p b is js

intersection :: RLE -> RLE -> RLE
intersection (RLE is) (RLE js) = RLE $ merge00 is js
  where
    merge00 [] js = []
    merge00 is [] = []
    merge00 (i@(a, b):is) (j@(c, d):js) = case compare a c of
      LT -> merge10 b is (j:js)
      EQ -> merge11 a b is d js
      GT -> merge01 (i:is) d js
    merge01 [] d js = []
    merge01 (i@(a, b):is) d js = case compare a d of
      LT -> merge11 a b is d js
      EQ -> merge10 b is js
      GT -> merge00 (i:is) js
    merge10 b is [] = []
    merge10 b is (j@(c, d):js) = case compare b c of
      LT -> merge00 is (j:js)
      EQ -> merge01 is d js
      GT -> merge11 c b is d js
    merge11 p b is d js = case compare b d of
      LT -> (p, b):merge01 is d js
      EQ -> (p, b):merge00 is js
      GT -> (p, d):merge10 b is js

isSubsetOf :: RLE -> RLE -> Bool
isSubsetOf (RLE is) (RLE js) = merge00 (toFlips is) (toFlips js)
  where
    toFlips = concatMap (\(a, b) -> [a, b])
    merge00 [] js = True
    merge00 is [] = False
    merge00 (i:is) (j:js) = case compare i j of
      LT -> False
      EQ -> merge11 is js
      GT -> merge01 (i:is) js
    merge01 [] js = True
    merge01 is [] = error "merge01"
    merge01 (i:is) (j:js) = case compare i j of
      LT -> merge11 is (j:js)
      EQ -> False
      GT -> merge00 (i:is) js
    merge11 [] js = error "merge11"
    merge11 is [] = error "merge11"
    merge11 (i:is) (j:js) = case compare i j of
      LT -> merge01 is (j:js)
      EQ -> merge00 is js
      GT -> False

member :: Int -> RLE -> Bool
member x (RLE is) = go is
  where
    go [] = False
    go ((a, b):is)
      | a > x = False
      | x < b = True
      | otherwise = go is

size :: RLE -> Int
size (RLE is) = foldl' (\n (a, b) -> n + b - a) 0 is

findAny :: RLE -> Int
findAny (RLE ((a, _):_)) = a

delete :: Int -> RLE -> RLE
delete x (RLE is) = RLE $ go is
  where
    go [] = []
    go w@(ab@(a, b):is)
      | x < a = w
      | x == a, x + 1 == b = is
      | x == a = (a + 1, b):is
      | x + 1 == b = (a, b - 1):is
      | x < b = (a, x):(x + 1, b):is
      | otherwise = ab:go is

null :: RLE -> Bool
null (RLE []) = True
null (RLE _) = False

shift :: Int -> RLE -> RLE
shift x (RLE is) = RLE (go is)
  where
    go [] = []
    go ((a, b):is) = (a + x, b + x):go is

consistency :: RLE -> ()
consistency (RLE is) = go is
  where
    go [] = ()
    go ((a, b):is)
      | a >= b = error "R1"
      | otherwise = go1 b is
    go1 l [] = ()
    go1 l ((a, b):is)
      | l >= a = error "R2"
      | a >= b = error "R3"
      | otherwise = go1 b is
