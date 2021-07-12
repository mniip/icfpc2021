{-# LANGUAGE DerivingStrategies, BangPatterns #-}
-- A set of ints encoded with Run-Length Encoding.
-- These admit extremely fast intersection and union operations, as well as 
module ICFPC.RLE where

import GHC.Exts

-- Run a b means the segment [a, b)
-- b is strictly greater than a, but strictly less than the a in the next Run if any
-- The spine is strict
data RLE = Stop | Run {-# UNPACK #-} !Int {-# UNPACK #-} !Int !RLE
  deriving stock (Eq, Ord, Show)

empty :: RLE
empty = Stop

-- encode an interval [a, b)
run :: Int -> Int -> RLE
run a b
  | a < b = Run a b Stop
  | otherwise = empty

singleton :: Int -> RLE
singleton a = Run a (a + 1) Stop

{-# INLINE toList #-}
toList :: RLE -> [Int]
toList is = build (go is)
  where
    go Stop cons nil  = nil
    go (Run a b is) cons nil = foldr cons (go is cons nil) [a .. b - 1]

{-# INLINE toRuns #-}
toRuns :: RLE -> [(Int, Int)]
toRuns is = build (go is)
  where
    go Stop cons nil = nil
    go (Run a b is) cons nil = cons (a, b) $ go is cons nil

-- strictly ascending
fromAscList :: [Int] -> RLE
fromAscList [] = empty
fromAscList (x:xs) = go x x xs
  where
    go !a !b [] = Run a (b + 1) Stop
    go !a !b (!c:xs)
      | c == b + 1 = go a (b + 1) xs
      | otherwise = Run a (b + 1) $ go c c xs

{-# INLINE union #-}
union :: RLE -> RLE -> RLE
union is js = merge00 is js
  where
    merge00 Stop js = js
    merge00 is Stop = is
    merge00 iw@(Run a b is) jw@(Run c d js) = case compare a c of
      LT -> merge10 a b is jw
      EQ -> merge11 a b is d js
      GT -> merge01 c iw d js
    merge01 p Stop d js = Run p d js
    merge01 p iw@(Run a b is) d js = case compare a d of
      LT -> merge11 p b is d js
      EQ -> merge10 p b is js
      GT -> Run p d $ merge00 iw js
    merge10 p b is Stop = Run p b is
    merge10 p b is jw@(Run c d js) = case compare b c of
      LT -> Run p b $ merge00 is jw
      EQ -> merge01 p is d js
      GT -> merge11 p b is d js
    merge11 p b is d js = case compare b d of
      LT -> merge01 p is d js
      EQ -> Run p b $merge00 is js
      GT -> merge10 p b is js

{-# INLINE intersection #-}
intersection :: RLE -> RLE -> RLE
intersection is js = merge00 is js
  where
    merge00 Stop js = Stop
    merge00 is Stop = Stop
    merge00 iw@(Run a b is) jw@(Run c d js) = case compare a c of
      LT -> merge10 b is jw
      EQ -> merge11 a b is d js
      GT -> merge01 iw d js
    merge01 Stop d js = Stop
    merge01 iw@(Run a b is) d js = case compare a d of
      LT -> merge11 a b is d js
      EQ -> merge10 b is js
      GT -> merge00 iw js
    merge10 b is Stop = Stop
    merge10 b is jw@(Run c d js) = case compare b c of
      LT -> merge00 is jw
      EQ -> merge01 is d js
      GT -> merge11 c b is d js
    merge11 p b is d js = case compare b d of
      LT -> Run p b $ merge01 is d js
      EQ -> Run p b $ merge00 is js
      GT -> Run p d $ merge10 b is js

{-# INLINE isSubsetOf #-}
isSubsetOf :: RLE -> RLE -> Bool
isSubsetOf is js = merge00 is js
  where
    merge00 Stop js = True
    merge00 is Stop = False
    merge00 iw@(Run a b is) j@(Run c d js) = case compare a c of
      LT -> False
      EQ -> merge11 b is d js
      GT -> merge01 iw d js
    merge01 Stop d js = True
    merge01 iw@(Run a b is) d js = case compare a d of
      LT -> merge11 b is d js
      EQ -> False
      GT -> merge00 iw js
    merge11 b is d js = case compare b d of
      LT -> merge01 is d js
      EQ -> merge00 is js
      GT -> False

member :: Int -> RLE -> Bool
member x is = go is
  where
    go Stop = False
    go (Run a b is)
      | a > x = False
      | x < b = True
      | otherwise = go is

size :: RLE -> Int
size = go 0
  where
    go !n Stop = n
    go !n (Run a b is) = go (n + b - a) is

-- If nonempty, return some element
findAny :: RLE -> Int
findAny (Run a _ _) = a

delete :: Int -> RLE -> RLE
delete x = go
  where
    go Stop = Stop
    go w@(Run a b is)
      | x < a = w
      | x == a, x + 1 == b = is
      | x == a = Run (a + 1) b is
      | x + 1 == b = Run a (b - 1) is
      | x < b = Run a x $ Run (x + 1) b is
      | otherwise = Run a b $ go is

null :: RLE -> Bool
null Stop = True
null (Run _ _ _) = False

-- Add x to all elements
{-# INLINE shift #-}
shift :: Int -> RLE -> RLE
shift x = go
  where
    go Stop = Stop
    go (Run a b is) = Run (a + x) (b + x) $ go is

-- Subtract all elements from x
reflect :: Int -> RLE -> RLE
reflect x = go Stop
  where
    !xp1 = x + 1
    go !r Stop = r
    go !r (Run a b is) = go (Run (xp1 - b) (xp1 - a) r) is

-- Check consistency of the data structure
consistency :: RLE -> ()
consistency = go
  where
    go Stop = ()
    go (Run a b is)
      | a >= b = error "R1"
      | otherwise = go1 b is
    go1 l Stop = ()
    go1 l (Run a b is)
      | l >= a = error "R2"
      | a >= b = error "R3"
      | otherwise = go1 b is
