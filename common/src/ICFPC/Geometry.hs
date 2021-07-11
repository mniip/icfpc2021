{-# LANGUAGE BangPatterns #-}
module ICFPC.Geometry where

import Data.List (minimumBy, maximumBy, sort, group, foldl')
import GHC.Exts
import Debug.Trace (traceShow)
import Data.Function (on)

import qualified Data.IntMap as IM

type Point = (Int, Int)

{-# INLINE (.+.) #-}
(.+.) :: Point -> Point -> Point
(x, y) .+. (x', y') = (nx, ny)
  where
    !nx = x + x'
    !ny = y + y'

{-# INLINE (.-.) #-}
(.-.) :: Point -> Point -> Point
(x, y) .-. (x', y') = (nx, ny)
  where
    !nx = x - x'
    !ny = y - y'

-- Signed area of a traingle (doubled)
area :: Point -> Point -> Point -> Int
area (x1, y1) (x2, y2) (x3, y3) = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

-- Check that two 1-dim intervals intersect
intersect1Dim :: Int -> Int -> Int -> Int -> Bool
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
pointOnInterval :: Int -> Int -> Int -> Bool
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
{-# INLINE cyclePairs #-}
cyclePairs :: [a] -> [(a, a)]
cyclePairs xs = foldr (\x f y z -> (y, x) : f x z) (\y z -> [(y, z)]) (tail xs) (head xs) (head xs)

-- Check that a point is inside a polygon
pointInPolygon :: Polygon -> Point -> Bool
pointInPolygon [] _ = True -- Counterintuitively, we consider an empty polygon to be infinite
pointInPolygon ps q@(qx, qy) = any (pointOnSegment q) (cyclePairs ps) || -- If q is on boundary, it's "inside"
                               foldl' edge False (cyclePairs ps)
    where edge f (a, b)
              | snd maxp <= qy || snd minp > qy = f
              | area minp maxp q > 0 = not f
              | otherwise = f -- Should not happen
                  where (minp, maxp) = if snd a > snd b then (b, a) else (a, b)

-- Check that a segment is inside a polygon
segmentInPolygon :: Polygon -> Segment -> Bool
segmentInPolygon ps s@(a, b) =
    pointInPolygon ps a &&
    pointInPolygon ps b && -- Both ends are inside
    all (not . properIntersectSegments s) (cyclePairs ps) && -- s does not intersect edges (but may touch)
    all (pointInPolygon doubleps) midpoints
        where edges = cyclePairs ps
              pointsOnSegment = map head . group . -- Remove doubles
                                sort $ -- Sort poits "in one direction"
                                ((filter (\p -> pointOnSegment p s) ps) ++ [a, b])
              doubleps = map (\(x, y) -> (2*x, 2*y)) ps
              midpoints = zipWith (.+.) pointsOnSegment (tail pointsOnSegment)

type Dist = Int

-- Distance squared
dist :: Point -> Point -> Dist
dist (x, y) (x', y') = (x-x')^2 + (y-y')^2

distSeg :: Segment -> Dist
distSeg (a, b) = dist a b

type Epsilon = Int

-- Given epsilon and original length, return a range of allowed lengths
stretchInterval :: Int -> Int -> (Int, Int)
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

dislikes :: Polygon -> Figure -> Int
dislikes hole pose = sum [ minimum [dist h v | v <- pose] | h <- hole]

isValid :: Epsilon -> Polygon -> [(Int, Int)] -> Figure -> Figure -> Bool
isValid eps hole edges origPose pose =
  all (segmentInPolygon hole) [(pose !! u, pose !! v) | (u, v) <- edges] &&
  and [canStretch eps (distSeg (origPose !! u, origPose !! v)) (pose !! u, pose !! v) == EQ | (u, v) <- edges]

-- Integer square root
isqrt :: Integral a => a -> a
isqrt 0 = 0
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

boundingBox :: Polygon -> (Point, Point)
boundingBox [] = ((-1000000, -1000000), (1000000, 1000000))
boundingBox ps = ((left, down), (right, up))
    where left = minimum $ map fst ps
          right = maximum $ map fst ps
          down = minimum $ map snd ps
          up = maximum $ map snd ps

intersectBBoxes :: (Point, Point) -> (Point, Point) -> (Point, Point)
intersectBBoxes ((left, down), (right, up)) ((left', down'), (right', up')) =
    ((max left left', max down down'), (min right right', min up up'))

-- Given epsilon and a point with original distance,
-- return a bounding box of the region within the stretching distance
pointBBox :: Epsilon -> (Point, Dist) -> (Point, Point)
pointBBox eps (p, d) = (p .-. v, p .+. v)
    where (_, maxl) = stretchInterval eps d
          horr = 1 + isqrt maxl
          v = (horr, horr)

-- Given a polygon, epsilon, a list of points with distances,
-- return all points inside the polygon which satisfy annula constraints
allowedPositions :: Polygon -> Epsilon -> [(Point, Dist)] -> [Point]
allowedPositions poly eps edges = insidePolyAndAnnula
    where ((left, down), (right, up)) = foldl' intersectBBoxes (boundingBox poly) (map (pointBBox eps) edges)
          points = [(x, y) | x <- [left..right], y <- [down..up]]
          goodEdge (q, d) p = canStretch eps d (q, p) == EQ
          insideAnnula = foldl (\a e -> filter (goodEdge e) a) points edges
          insidePolyAndAnnula = filter (pointInPolygon poly) insideAnnula

-- A version of isValid
valid :: Epsilon -> Polygon -> [(Int, Int, Dist)] -> [Point] -> (Bool, Bool)
valid eps bs es vs = (all (\(u, v, d) -> segmentInPolygon bs (vs !! u, vs !! v)) es,
                      all (\(u, v, d) -> canStretch eps d (vs !! u, vs !! v) == EQ) es)

-- Move point to the best "green" location.
-- Returns new figure
improvePoint :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> Int -> Maybe [Point]
improvePoint eps is xs bs i = if null variants then Nothing else Just . snd $ minimumBy (compare `on` fst) variants
    where coords = allowedPositions bs eps jss
          jss = [(xs !! j, d) | (i', j, d) <- is, i' == i] ++ [(xs !! j, d) | (j, i', d) <- is, i' == i]
          replace p = let (before, after) = splitAt i xs in before ++ p:tail after
          variants = filter (\(_, ys) -> fst (valid eps bs is ys)) $ map (\p -> (dislikes bs (replace p), replace p)) coords

-- Improve all points in order
improvePoints :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> [Point]
improvePoints eps is xs bs = foldl go xs $ zip [0..] xs
    where go ys (i,p) = case improvePoint eps is ys bs i of
                             Nothing -> ys
                             Just ys' -> ys'

-- Try improving points until it stops working
totallyImprove :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> [Point]
totallyImprove eps is xs bs = go (dislikes bs xs) xs
    where go d ys = let ys' = improvePoints eps is ys bs
                        d' = dislikes bs ys'
                    in if d' < d then go d' ys' else ys

-- Move a point away from the center of mass
putPointAway :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> Int -> Maybe [Point]
putPointAway eps is xs bs i = if null coords 
                              then Nothing
                              else Just . replace $ maximumBy (compare `on` (\p -> sum $ map (isqrt . dist p) xs)) coords
    where len = length xs
          mass = (sum (map fst xs) `div` len, sum (map snd xs) `div` len)
          coords = allowedPositions [] (2*eps) jss
          jss = [(xs !! j, d) | (i', j, d) <- is, i' == i] ++ [(xs !! j, d) | (j, i', d) <- is, i' == i]
          replace p = let (before, after) = splitAt i xs in before ++ p:tail after

putPointsAway :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> [Point]
putPointsAway eps is xs bs = foldl go xs $ zip [0..] xs
    where go ys (i,p) = case putPointAway eps is ys bs i of
                             Nothing -> ys
                             Just ys' -> ys'

vdiv (x,y) l = ((x+1) `quot` l, (y+1) `quot` l)
vmul (x,y) l = (x*l, y*l)

{-
adjustPoint :: Epsilon -> Dist -> Point -> Point -> Point
adjustPoint eps d p q =
    let qp = q .-. p
        len = (2*(isqrt $ dist q p)) `div` 3
    in case canStretch eps d (p, q) of
           EQ -> p
           LT -> p .-. (qp `vdiv` (1+len))
           GT -> p .+. (qp `vdiv` (1+len))

adjustPoints :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> [Point]
adjustPoints eps is xs = zipWith go [0..] xs
    where is' = is ++ map (\(a, b, c) -> (b, a, c)) is
          go i q = foldl (\p (_, j, d) -> adjustPoint eps d p (xs !! j)) q (filter (\(j, _, _) -> i == j) is')
          -}

-- Stretch or contract edges
adjustPoints :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> [Point]
adjustPoints eps is xs poly = map snd . IM.toList $ foldl go ps is
    where ps = IM.fromList $ zip [0..] xs
          go :: IM.IntMap Point -> (Int, Int, Dist) -> IM.IntMap Point
          go ys (i, j, d) =
              let p = ys IM.! i
                  q = ys IM.! j
                  qp = q .-. p
                  len = 1 + ((2*(isqrt $ dist q p)) `div` 3)
                  maybeInsert k r r' mp | r `elem` poly || r' `elem` poly = mp
                                        | otherwise = IM.insert k r' mp
              in case canStretch eps d (p, q) of
                     EQ -> ys
                     LT -> maybeInsert i p (p .-. (qp `vdiv` len)) .
                           maybeInsert j q (q .+. (qp `vdiv` len)) $ ys
                     GT -> maybeInsert i p (p .+. (qp `vdiv` len)) .
                           maybeInsert j q (q .-. (qp `vdiv` len)) $ ys

