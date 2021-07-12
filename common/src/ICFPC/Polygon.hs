{-# LANGUAGE BangPatterns, DerivingStrategies, ViewPatterns #-}
-- Utilities for manipulating polygons: walking around vertices and filling them with integer points
module ICFPC.Polygon where

import Data.Ord
import Data.Maybe
import Data.Ratio
import Data.List

import ICFPC.DbList
import ICFPC.Vector
import ICFPC.Rational
import qualified ICFPC.RLE as R
import qualified ICFPC.RLE2D as R2

-- A doubly linked list (ring) of vertices
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

polygonVertices :: Polygon -> [V2]
polygonVertices (Polygon ps) = pVertex <$> ps

instance Show Polygon where
  showsPrec d (Polygon ps) = showParen (d > 9) $ showString "mkPolygon " . showsPrec 10 (pVertex <$> ps)

mkPolygon :: [V2] -> Polygon
mkPolygon vs = Polygon $ mkCycDbList (\prev v next -> PolyVertex v prev next) vs

-- returns 2x the signed area of the polygon
polySignedArea2x :: Polygon -> Int
polySignedArea2x (Polygon ((pVertex -> a):vs)) = sum [signedArea (a .-. pVertex v) (pVertex (pPrevVertex v) .-. a) | v <- vs]
polySignedArea2x _ = 0

-- Ensure that the polygon is CCW
mkPolyCCW :: Polygon -> Polygon
mkPolyCCW poly@(Polygon vs)
  | polySignedArea2x poly >= 0 = poly
  | otherwise = mkPolygon $ reverse $ pVertex <$> vs

-- Return the diagonal of the bounding rectangle of the polygon
polyBoundingBox :: Polygon -> S2
polyBoundingBox (Polygon (map pVertex -> (v:vs))) = foldl' minMax (S2V2 v v) vs
  where
    minMax (S2 x1 y1 x2 y2) (V2 x y) = S2 (min x x1) (min y y1) (max x x2) (max y y2)

-- Is a point in the triangle (incl edges)
pointInTriangle :: (V2, V2, V2) -> V2 -> Bool
pointInTriangle (v1, v2, v3) pt =
  (d1 >= 0 && d2 >= 0 && d3 >= 0) ||
  (d1 <= 0 && d2 <= 0 && d3 <= 0)
  where
    !d1 = signedArea (v1 .-. pt) (v2 .-. pt)
    !d2 = signedArea (v2 .-. pt) (v3 .-. pt)
    !d3 = signedArea (v3 .-. pt) (v1 .-. pt)

-- Partition a non-convex polygon into triangles. Input assumed CCW
triangulate :: Polygon -> [(V2, V2, V2)]
triangulate (mkPolyCCW -> Polygon ps) = go (pVertex <$> ps) []
  where
    go :: [V2] -> [(V2, V2, V2)] -> [(V2, V2, V2)]
    go [v1, v2, v3] !acc = (v1, v2, v3):acc
    go poly !acc = case find isEar $ cycTriples poly of
        Nothing -> acc
        Just (prev, ear, next) -> go (delete ear poly) ((prev, ear, next):acc)
      where
        isEar (prev, cur, next) = onTheRight (prev, next) cur &&
          not (any (pointInTriangle (prev, next, cur)) $ poly \\ [prev, cur, next])

    onTheRight (p1, p2) p = signedArea (p .-. p1) (p2 .-. p1) > 0

-- Return the integer points that lie inside the polygon. Input assumed CCW
computePolygonInternals :: Polygon -> R2.RLE2D
computePolygonInternals poly = R2.unions [fillTriangle a b c | (a, b, c) <- triangulate poly]


-- Fill the pixels of a single triangle. Include the integer points that lie on the edges.
fillTriangle :: V2 -> V2 -> V2 -> R2.RLE2D
fillTriangle !a !b !c = R2.fromAscList $ if signedArea (mid .-. bottom) (top .-. bottom) >= 0
  then [(y, R.run (segMin bottom top y) (segMax bottom mid y + 1)) | y <- [by..my]] <> {- 1 -}
       [(y, R.run (segMin bottom top y) (segMax mid top y + 1)) | y <- [my+1..ty]]
  else [(y, R.run (segMin bottom mid y) (segMax bottom top y + 1)) | y <- [by..my]] <> {- 2 -}
       [(y, R.run (segMin mid top y) (segMax bottom top y + 1)) | y <- [my+1..ty]]
--    T         T
--    |\       /|
-- 1: | M  2: M |
--    |/       \|
-- y  B         B
-- ^
-- +>x
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

-- Fill the pixels of a triangle whose vertices are rational
fillTriangleQ :: Q2 -> Q2 -> Q2 -> R2.RLE2D
fillTriangleQ !a !b !c = R2.fromAscList $ if signedAreaQ (mid ~-~ bottom) (top ~-~ bottom) >= 0
  then [(y, R.run (segMin bottom top y) (segMax bottom mid y + 1)) | y <- [qCeil by .. qFloor my]] <>
       [(y, R.run (segMin bottom top y) (segMax mid top y + 1)) | y <- [qFloor my + 1 .. qFloor ty]]
  else [(y, R.run (segMin bottom mid y) (segMax bottom top y + 1)) | y <- [qCeil by .. qFloor my]] <>
       [(y, R.run (segMin mid top y) (segMax bottom top y + 1)) | y <- [qFloor my + 1 .. qFloor ty]]
  where
    [!bottom@(Q2 _ by), !mid@(Q2 _ my), !top@(Q2 _ ty)] = sortBy (comparing $ \(Q2 _ y) -> y) [a, b, c]

    -- by => ay
    segMin (Q2 ax ay) (Q2 bx by) !y
      | ay == by = qCeil $ min ax bx
      | otherwise = qCeil $ bx + (by - toInteger y % 1) * (ax - bx) / (by - ay)

    -- by => ay
    segMax (Q2 ax ay) (Q2 bx by) !y
      | ay == by = qFloor $ max ax bx
      | otherwise = qFloor $ bx + (by - toInteger y % 1) * (ax - bx) / (by - ay)

-- signed angle from a to b is in [0, pi)
angle0ToPiExcl :: V2 -> V2 -> Bool
angle0ToPiExcl a b = (signedArea a b, a `dot` b) >= (0, 0)

-- When considering whether a point is obstructed by the "outsides", it's possible that everythign to the left/right is
-- obstructed but not the point itself: we're obstructing by an open set, and the target point could be on the boundary
data Obstruction = ObstrFull | ObstrCW | ObstrCCW
  deriving stock (Eq, Ord, Show)

-- Compute the points of a polygon that are "visible" from a given point (to which a segment could be drawn)
--
-- What we do is iterate through the vertices in the order that's CCW around the source point, and to every vertex we
-- cast a ray, and we keep track of not only when the target vertex is obstructed, but also when everything to the left
-- or right is obstructed, if e.g. the ray is tangent to an edge, or passes through a vertex without going outside the
-- polygon.
--
-- In then end the region is visibility is a bunch of triangles with rational coordinates. We take their unions.
computePolygonVisibility :: Polygon -> V2 -> R2.RLE2D
computePolygonVisibility (Polygon vs) org = R2.unions [fillTriangleQ (v2ToQ2 1 org) a b | (a, b) <- cycPairs star]
  where
    vertices = sortBy (comparing angle) $ filter (/= org) $ pVertex <$> vs

    star = case find ((== org) . pVertex) vs of
      Nothing -> concatMap castRay vertices
      -- the source point is one of the vertices, have to reorder the rest of the vertices around it
      Just pv
        | let !prev = angle $ pVertex $ pPrevVertex pv
        , let !next = angle $ pVertex $ pNextVertex pv
        -> if next <= prev
           then v2ToQ2 1 org : concatMap castRay (filter (\v -> angle v >= next && angle v <= prev) vertices)
           else let (xs, rs) = span (\v -> angle v <= prev) vertices
                    ys = dropWhile (\v -> angle v < next) rs
                in v2ToQ2 1 org : concatMap castRay (ys ++ xs)

    -- return quadrant and y/x within that quadrant - for sorting
    angle :: V2 -> (Int, Ratio Int)
    angle v = case v .-. org of
      V2 dx dy
        | dx > 0, dy >= 0 -> (0, dy % dx)
        | dy > 0, dx <= 0 -> (1, -dx % dy)
        | dx < 0, dy <= 0 -> (2, dy % dx)
        | dy < 0, dx >= 0 -> (3, -dx % dy)
        | otherwise -> error "angle of 0"

    castRay targ
      | otherwise = collect Nothing Nothing Nothing $ mapMaybe (rayIntersection targ) vs
      where
        collect !cw !here !ccw [] = (v2ToQ2 1 org ~+~) <$> catMaybes [cw, here, ccw]
        collect !cw !here !ccw ((ObstrFull, q):os) = collect (upd q cw) (upd q here) (upd q ccw) os
        collect !cw !here !ccw ((ObstrCW, q):os) = collect (upd q cw) here ccw os
        collect !cw !here !ccw ((ObstrCCW, q):os) = collect cw here (upd q ccw) os
        upd q1 Nothing = Just q1
        upd q1 (Just q2) = Just $ if q1 `dotQ` q1 < q2 `dotQ` q2 then q1 else q2

    -- Check whether a ray cast from "org" to "targ" intersects the half-open segment from "pv" to the next vertex
    -- after "pv", and if so - what kind of obstruction it is
    rayIntersection targ pv
      | signedArea (a .-. org) dir == 0 && adist >= 0 -- hit a vertex
      = case (angle0ToPiExcl dir (a .-. c), angle0ToPiExcl (a .-. b) dir) of
        (True, True) -> Just (ObstrFull, v2ToQ2 1 $ a .-. org) {- 1 -}
        (True, False) | adist > 0 -> Just (ObstrCW, v2ToQ2 1 $ a .-. org) {- 2 -}
        (False, True) | adist > 0 -> Just (ObstrCCW, v2ToQ2 1 $ a .-. org) {- 2 -}
        _ -> Nothing
      --   targ     targ        targ
      --     ^        ^  b     c  |
      --  1: |     2: | ^   3:  \ |
      --     |        |/         v|
      -- b<--a<--c    a           a
      --     |        |^         /|
      --     |        | \       v |
      --     |        |  c     b  |
      --    org      org         org
      --
      | not $ separatesStrictly (S2V2 org targ) a b = Nothing
      -- now det /= 0
      -- else hit the interior of an edge
      | signedArea dir (b .-. a) <= 0 = Nothing -- hit it from behind
      | let d = signedArea (a .-. org) (b .-. org) .*. dir
      , productSign det (d `dot` dir) /= LT = Just $ (ObstrFull, v2ToQ2 det d)
      | otherwise = Nothing
      where
        !a = pVertex pv
        !adist = (a .-. org) `dot` dir
        !b = pVertex $ pNextVertex pv
        bdist = (b .-. org) `dot` dir
        c = pVertex $ pPrevVertex pv
        !dir = targ .-. org
        det = signedArea (a .-. b) dir
