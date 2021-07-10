{-# LANGUAGE BangPatterns, DerivingStrategies, ViewPatterns #-}

module ICFPC.Polygon where

import qualified Data.IntMap.Strict as IM
import Data.List (sortBy)
import Data.Ord
import Data.Ratio

import ICFPC.Vector
import qualified ICFPC.RLE as RLE

data PolyVertex = PolyVertex
  { pVertex :: {-# UNPACK #-} !V2
  , pPrevVertex :: PolyVertex
  , pNextVertex :: PolyVertex
  }

{-# INLINE pNextSegment #-}
pNextSegment :: PolyVertex -> S2
pNextSegment pv = S2V2 (pVertex pv) (pVertex $ pNextVertex pv)

{-# INLINE pPrevSegment #-}
pPrevSegment :: PolyVertex -> S2
pPrevSegment pv = S2V2 (pVertex $ pPrevVertex pv) (pVertex pv)

-- assumed no self-intersections
newtype Polygon = Polygon [PolyVertex]

instance Show Polygon where
  showsPrec d (Polygon ps) = showParen (d > 9) $ showString "mkPolygon " . showsPrec 10 (pVertex <$> ps)

mkPolygon :: [V2] -> Polygon
mkPolygon [] = Polygon []
mkPolygon [v] = let node = PolyVertex v node node in Polygon [node]
mkPolygon (v:u:us) = let
  !(last, second, list) = go first first u us
  first = PolyVertex v last second
  in Polygon (first:list)
  where
    go first prev v [] = let node = PolyVertex v prev first in (node, node, [node])
    go first prev v (u:us) = let
      !(last, next, list) = go first node u us
      node = PolyVertex v prev next
      in (last, node, node:list)

-- returns 2x the signed area of the polygon
polySignedArea2x :: Polygon -> Int
polySignedArea2x (Polygon ((pVertex -> a):(pVertex -> b):vs)) = sum [signedArea ba (pVertex v .-. a) | v <- vs]
  where !ba = b .-. a
polySignedArea2x _ = 0

-- map from y to run of x's
--    T         T
--    |\       /|
-- 1: | M  2: M |
--    |/       \|
-- y  B         B
-- ^
-- +>x
fillTriangle :: V2 -> V2 -> V2 -> IM.IntMap RLE.RLESeq
fillTriangle !a !b !c = IM.fromAscList $ if signedArea (mid .-. bottom) (top .-. bottom) >= 0
  then [(y, RLE.run (segMin bottom top y) (segMax bottom mid y + 1)) | y <- [by..my]] <> {- 1 -}
       [(y, RLE.run (segMin bottom top y) (segMax mid top y + 1)) | y <- [my+1..ty]]
  else [(y, RLE.run (segMin bottom mid y) (segMax bottom top y + 1)) | y <- [by..my]] <> {- 2 -}
       [(y, RLE.run (segMin mid top y) (segMax bottom top y + 1)) | y <- [my+1..ty]]
  where
    [!bottom@(V2 _ by), !mid@(V2 _ my), !top@(V2 _ ty)] = sortBy (comparing $ \(V2 _ y) -> y) [a, b, c]

    -- by => ay
    segMin (V2 ax ay) (V2 bx by) !y
      | ay == by = min ax bx
      | otherwise = bx + ceilDiv ((by - y) * (ax - bx)) (by - ay)

    -- by => ay
    segMax (V2 ax ay) (V2 bx by) !y
      | ay == by = max ax bx
      | otherwise = bx + floorDiv ((by - y) * (ax - bx)) (by - ay)

    floorDiv p q = p `div` q
    ceilDiv p q = -((-p) `div` q)

data Q2 = Q2 {-# UNPACK #-} !(Ratio Int) !(Ratio Int)
  deriving stock (Eq, Ord, Show)

fillTriangleQ :: Q2 -> Q2 -> Q2 -> IM.IntMap RLE.RLESeq
fillTriangleQ !a !b !c = IM.fromAscList $ if signedAreaQ (mid ~-~ bottom) (top ~-~ bottom) >= 0
  then [(y, RLE.run (segMin bottom top y) (segMax bottom mid y + 1)) | y <- [qCeil by .. qFloor my]] <>
       [(y, RLE.run (segMin bottom top y) (segMax mid top y + 1)) | y <- [qFloor my + 1 .. qFloor ty]]
  else [(y, RLE.run (segMin bottom mid y) (segMax bottom top y + 1)) | y <- [qCeil by .. qFloor my]] <>
       [(y, RLE.run (segMin mid top y) (segMax bottom top y + 1)) | y <- [qCeil my + 1 .. qFloor ty]]
  where
    [!bottom@(Q2 _ by), !mid@(Q2 _ my), !top@(Q2 _ ty)] = sortBy (comparing $ \(Q2 _ y) -> y) [a, b, c]

    -- by => ay
    segMin (Q2 ax ay) (Q2 bx by) !y
      | ay == by = qCeil $ min ax bx
      | otherwise = qCeil $ bx + (by - y % 1) * (ax - bx) / (by - ay)

    -- by => ay
    segMax (Q2 ax ay) (Q2 bx by) !y
      | ay == by = qFloor $ max ax bx
      | otherwise = qFloor $ bx + (by - y % 1) * (ax - bx) / (by - ay)

    qFloor q = numerator q `div` denominator q
    qCeil q = -((-numerator q) `div` denominator q)

    Q2 x1 y1 ~-~ Q2 x2 y2 = Q2 (x1 - x2) (y1 - y2)

    signedAreaQ (Q2 x1 y1) (Q2 x2 y2) = x1 * y2 - y1 * x2
