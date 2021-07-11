{-# LANGUAGE DerivingStrategies, BangPatterns #-}
module ICFPC.RLE where

import Data.List (foldl')

-- run length encoded set of ints
-- a pair (a, b) in the sequence encodes an interval [a, b)
-- b must be greater than a, but smaller than the next a in the list
data Run = Run {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

newtype RLE = RLE [Run]
  deriving stock (Eq, Ord, Show)

{-# INLINE (!:) #-}
(!:) :: Run -> [Run] -> [Run]
(!:) !x !xs = x : xs
infixr 5 !:

empty :: RLE
empty = RLE []

-- encode an interval [a, b)
run :: Int -> Int -> RLE
run a b
  | a < b = RLE [Run a b]
  | otherwise = empty

singleton :: Int -> RLE
singleton a = RLE [Run a (a + 1)]

toList :: RLE -> [Int]
toList (RLE is) = [x | Run a b <- is, x <- [a..b-1]]

toRuns :: RLE -> [(Int, Int)]
toRuns (RLE is) = [(a, b) | Run a b <- is]

-- strictly ascending
fromAscList :: [Int] -> RLE
fromAscList [] = empty
fromAscList (x:xs) = RLE $ go x x xs
  where
    go !a !b [] = [Run a (b + 1)]
    go !a !b (!c:xs)
      | c == b + 1 = go a (b + 1) xs
      | otherwise = Run a (b + 1)!:go c c xs

union :: RLE -> RLE -> RLE
union (RLE is) (RLE js) = RLE $ merge00 is js
  where
    merge00 [] js = js
    merge00 is [] = is
    merge00 (i@(Run a b):is) (j@(Run c d):js) = case compare a c of
      LT -> merge10 a b is (j:js)
      EQ -> merge11 a b is d js
      GT -> merge01 c (i:is) d js
    merge01 p [] d js = (Run p d)!:js
    merge01 p (i@(Run a b):is) d js = case compare a d of
      LT -> merge11 p b is d js
      EQ -> merge10 p b is js
      GT -> (Run p d)!:merge00 (i:is) js
    merge10 p b is [] = (Run p b)!:is
    merge10 p b is (j@(Run c d):js) = case compare b c of
      LT -> (Run p b)!:merge00 is (j:js)
      EQ -> merge01 p is d js
      GT -> merge11 p b is d js
    merge11 p b is d js = case compare b d of
      LT -> merge01 p is d js
      EQ -> (Run p b)!:merge00 is js
      GT -> merge10 p b is js

intersection :: RLE -> RLE -> RLE
intersection (RLE is) (RLE js) = RLE $ merge00 is js
  where
    merge00 [] js = []
    merge00 is [] = []
    merge00 (i@(Run a b):is) (j@(Run c d):js) = case compare a c of
      LT -> merge10 b is (j:js)
      EQ -> merge11 a b is d js
      GT -> merge01 (i:is) d js
    merge01 [] d js = []
    merge01 (i@(Run a b):is) d js = case compare a d of
      LT -> merge11 a b is d js
      EQ -> merge10 b is js
      GT -> merge00 (i:is) js
    merge10 b is [] = []
    merge10 b is (j@(Run c d):js) = case compare b c of
      LT -> merge00 is (j:js)
      EQ -> merge01 is d js
      GT -> merge11 c b is d js
    merge11 p b is d js = case compare b d of
      LT -> (Run p b)!:merge01 is d js
      EQ -> (Run p b)!:merge00 is js
      GT -> (Run p d)!:merge10 b is js

isSubsetOf :: RLE -> RLE -> Bool
isSubsetOf (RLE is) (RLE js) = merge00 is js
  where
    merge00 [] js = True
    merge00 is [] = False
    merge00 (i@(Run a b):is) (j@(Run c d):js) = case compare a c of
      LT -> False
      EQ -> merge11 b is d js
      GT -> merge01 (i:is) d js
    merge01 [] d js = True
    merge01 (i@(Run a b):is) d js = case compare a d of
      LT -> merge11 b is d js
      EQ -> False
      GT -> merge00 (i:is) js
    merge11 b is d js = case compare b d of
      LT -> merge01 is d js
      EQ -> merge00 is js
      GT -> False

member :: Int -> RLE -> Bool
member x (RLE is) = go is
  where
    go [] = False
    go (Run a b:is)
      | a > x = False
      | x < b = True
      | otherwise = go is

size :: RLE -> Int
size (RLE is) = foldl' (\n (Run a b) -> n + b - a) 0 is

findAny :: RLE -> Int
findAny (RLE (Run a _:_)) = a

delete :: Int -> RLE -> RLE
delete x (RLE is) = RLE $ go is
  where
    go [] = []
    go w@(ab@(Run a b):is)
      | x < a = w
      | x == a, x + 1 == b = is
      | x == a = Run (a + 1) b:is
      | x + 1 == b = Run a (b - 1):is
      | x < b = Run a x!:Run (x + 1) b!:is
      | otherwise = ab!:go is

null :: RLE -> Bool
null (RLE []) = True
null (RLE _) = False

shift :: Int -> RLE -> RLE
shift x (RLE is) = RLE (go is)
  where
    go [] = []
    go (Run a b:is) = Run (a + x) (b + x)!:go is

consistency :: RLE -> ()
consistency (RLE is) = go is
  where
    go [] = ()
    go (Run a b:is)
      | a >= b = error "R1"
      | otherwise = go1 b is
    go1 l [] = ()
    go1 l (Run a b:is)
      | l >= a = error "R2"
      | a >= b = error "R3"
      | otherwise = go1 b is
