module ICFPC.Geometry where

import Debug.Trace (traceShow)

type Point = (Integer, Integer)

(.+.) :: Point -> Point -> Point
(x, y) .+. (x', y') = (x+x', y+y')

(.-.) :: Point -> Point -> Point
(x, y) .-. (x', y') = (x-x', y-y')

-- Signed area of a traingle (doubled)
area :: Point -> Point -> Point -> Integer
area (x1, y1) (x2, y2) (x3, y3) = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

-- Check that two 1-dim intervals intersect
intersect1Dim :: Integer -> Integer -> Integer -> Integer -> Bool
intersect1Dim a b c d = max l1 l2 <= min r1 r2
    where l1 = min a b
          r1 = max a b
          l2 = min c d
          r2 = max c d

type Segment = (Point, Point)

-- Check that two 2D segments intersect
intersectSegments :: Segment -> Segment -> Bool
intersectSegments (a, b) (c, d) =
    intersect1Dim (fst a) (fst b) (fst c) (fst d) &&
    intersect1Dim (snd a) (snd b) (snd c) (snd d) && -- Bounding box
    (area a b c) * (area a b d) <= 0 && -- d and c are separated by ab
    (area c d a) * (area c d b) <= 0 -- a and b are separated by dc

-- Check that p lies in [a, b]
pointOnInterval :: Integer -> Integer -> Integer -> Bool
pointOnInterval p a b = (min a b) <= p && p <= (max a b)

-- Check that point belongs to a 2D segment
pointOnSegment :: Point -> Segment -> Bool
pointOnSegment p (a, b) =
    pointOnInterval (fst p) (fst a) (fst b) &&
    pointOnInterval (snd p) (snd a) (snd b) && -- Bounding box
    area p a b == 0 -- Triangle is flat

-- Check that two 2D segments intersect nontrivially
properIntersectSegments :: Segment -> Segment -> Bool
properIntersectSegments ab@(a, b) cd@(c, d) =
    intersectSegments ab cd && -- Segments intersect
    not (any (\p -> pointOnSegment p cd) [a, b]) && -- a, b not on cd
    not (any (\p -> pointOnSegment p ab) [c, d])    -- c, d not on ab

type Polygon = [Point]

-- [1, 2, 3] -> [(1,2), (2,3), (3,1)]
cyclePairs :: [a] -> [(a, a)]
cyclePairs ls = zip ls (tail ls ++ take 1 ls)

-- Check that a point is inside a polygon
pointInPolygon :: Polygon -> Point -> Bool
pointInPolygon ps q@(qx, qy) = any (pointOnSegment q) edges || -- If q is on boundary, it's "inside"
                               not (foldl edge True edges)
    where edges = cyclePairs ps
          edge f (a, b)
              | snd maxp <= snd q || snd minp > snd q = f
              | area minp maxp q > 0 = not f
              | otherwise = f -- Should not happen
                  where (minp, maxp) = if snd a > snd b then (b, a) else (a, b)

-- Check that a segment is inside a polygon
segmentInPolygon :: Polygon -> Segment -> Bool
segmentInPolygon ps s@(a, b) = pointInPolygon ps a &&
                               pointInPolygon ps b && -- Both ends are inside
                               not (any (properIntersectSegments s) (cyclePairs ps)) -- ps does not intersect edges (but may touch)

type Dist = Integer

-- Distance squared
dist :: Point -> Point -> Dist
dist (x, y) (x', y') = (x-x')^2 + (y-y')^2

distSeg :: Segment -> Dist
distSeg (a, b) = dist a b

type Epsilon = Integer

-- Given epsilon and original length, return a range of allowed lengths
stretchInterval :: Integer -> Integer -> (Integer, Integer)
stretchInterval eps d = (max 0 lower, upper)
    where fdiv a b = negate ((negate a) `div` b)
          lower = (d*(1000000 - eps)) `fdiv` 1000000
          upper = (d*(1000000 + eps)) `div` 1000000

-- Given epsilon, original length and a segment, check if it can stretch
canStretch :: Epsilon -> Dist -> Segment -> Ordering
canStretch eps d s
  | 1000000 * (d' - d) > eps * d = GT
  | 1000000 * (d - d') > eps * d = LT
  | otherwise = EQ
  where d' = distSeg s

type Figure = [Point]

dislikes :: Polygon -> Figure -> Integer
dislikes hole pose = sum [ minimum [dist h v | v <- pose] | h <- hole]

-- Integer square root
isqrt :: Integral a => a -> a
isqrt n = go (next n) n
    where next xk = (xk + (n `div` xk)) `div` 2
          go xk xkm1 = let xkp1 = next xk
                       in if xkp1 == xk || xkp1 == xkm1 
                          then min xk xkm1
                          else go xkp1 xk

-- Given epsilon and original length, list all points within stretching distance
stretchAnnulus :: Epsilon -> Dist -> [Point]
stretchAnnulus eps d = rightUpper ++
                       (reflX rightUpper) ++ -- upper left
                       (reflY rightUpper) ++ -- down right
                       (reflX $ reflY rightUpper) ++ -- down left
                       cross
    where (lower, upper) = stretchInterval eps d
          sqrtUpper = isqrt upper
          inside = filter (\q -> lower <= dist (0,0) q && dist (0,0) q <= upper)
          rightUpper = inside [(x, y) | x <- [1..sqrtUpper], y <- [1..sqrtUpper]]
          cross = inside $ [(x, 0) | x <- [-sqrtUpper .. sqrtUpper]] ++
                           [(0, y) | y <- [-sqrtUpper .. sqrtUpper], y /= 0]
          reflX = map (\(x, y) -> (-x, y))
          reflY = map (\(x, y) -> (x, -y))