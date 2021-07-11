module ICFPC.RLE2D where

import qualified Data.IntMap.Strict as IM

import ICFPC.Vector
import qualified ICFPC.RLE as R

newtype RLE2D = RLE2D (IM.IntMap R.RLE)
  deriving (Eq, Ord, Show)

member :: V2 -> RLE2D -> Bool
member (V2 x y) (RLE2D m) = case IM.lookup y m of
  Nothing -> False
  Just s -> x `R.member` s

fromAscList :: [(Int, R.RLE)] -> RLE2D
fromAscList xs = RLE2D $ IM.fromAscList xs

fromList :: [V2] -> RLE2D
fromList xs = RLE2D $ IM.unionsWith R.union [IM.singleton y $ R.singleton x | V2 x y <- xs]

toList :: RLE2D -> [V2]
toList (RLE2D m) = [V2 x y | (y, s) <- IM.toList m, x <- R.toList s]

unions :: [RLE2D] -> RLE2D
unions xss = RLE2D $ IM.unionsWith R.union [xs | RLE2D xs <- xss]

-- [x1, x2) and y
toRuns :: RLE2D -> [(Int, Int, Int)]
toRuns (RLE2D m) = [(x1, x2, y) | (y, s) <- IM.toList m, (x1, x2) <- R.toRuns s]

empty :: RLE2D
empty = RLE2D IM.empty

isSubsetOf :: RLE2D -> RLE2D -> Bool
isSubsetOf (RLE2D m1) (RLE2D m2) = IM.isSubmapOfBy R.isSubsetOf m1 m2

intersection :: RLE2D -> RLE2D -> RLE2D
intersection (RLE2D m1) (RLE2D m2) = RLE2D (IM.filter (not . R.null) $ IM.intersectionWith R.intersection m1 m2)

size :: RLE2D -> Int
size (RLE2D m) = IM.foldl' (\n s -> n + R.size s) 0 m

singleton :: V2 -> RLE2D
singleton (V2 x y) = RLE2D $ IM.singleton y $ R.singleton x

findAny :: RLE2D -> V2
findAny (RLE2D m) = case head $ IM.toList m of
  (y, s) -> V2 (R.findAny s) y

delete :: V2 -> RLE2D -> RLE2D
delete (V2 x y) (RLE2D m) = RLE2D $ IM.update (\s -> let s' = R.delete x s in if R.null s' then Nothing else Just s') y m

shift :: V2 -> RLE2D -> RLE2D
shift (V2 x y) (RLE2D m) = RLE2D $ IM.mapKeysMonotonic (+ y) $ IM.map (R.shift x) m

consistency :: RLE2D -> ()
consistency (RLE2D m) = IM.foldl' (\() s -> if R.null s then error "N" else R.consistency s) () m
