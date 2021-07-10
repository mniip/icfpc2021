{-# LANGUAGE BangPatterns, ViewPatterns #-}

module ICFPC.Polygon where

import ICFPC.Vector

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
