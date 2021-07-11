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
