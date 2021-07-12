{-# LANGUAGE BangPatterns #-}
module ICFPC.Geometry where

import Data.List (tails, inits, sortBy, minimumBy, maximumBy, sort, group, foldl', intersect)
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

incident :: Eq a => (a, a) -> (a, a) -> Bool
incident (a, b) (c, d) = a == c || a == d || b == c || b == d

-- List indices of figure vertices, s.t. a corresponding edge matches a given segment
matchingSides :: Epsilon -> [(Int, Int, Dist)] -> Segment -> [(Int, Int)]
matchingSides eps is s = map (\(i, j, _) -> (i, j)) $ filter (\(i, j, d) -> canStretch eps d s == EQ) is

longestChain :: Epsilon -> [(Int, Int, Dist)] -> Polygon -> [(Int, Int)]
longestChain eps is poly = if null chainTree || null (head chainTree)
                           then []
                           else maximumBy (compare `on` length) [unroll [h] (tail chainTree) | h <- head chainTree]
    where sides = cyclePairs poly
          chainTree = go [] sides
          go :: [[(Int, Int)]] -> [Segment] -> [[(Int, Int)]]
          go accum [] = accum
          go accum (s:ss) = let ms = matchingSides eps is s
                                prev = head accum
                                cms = filter (\e -> any (incident e) prev) ms
                            in if null accum
                               then go [ms] ss
                               else if null cms then accum else go (cms:accum) ss
          unroll :: [(Int, Int)] -> [[(Int, Int)]] -> [(Int, Int)]
          unroll [] _ = error "Empty accumulator"
          unroll accum [] = accum
          unroll accum (s:ss) =
              let (left, _) = head accum
                  inc = filter (\(a, b) -> a == left || b == left) s
                  e = head inc
                  swap (a, b) | a == left = (b, a)
                              | otherwise = (a, b)
              in if null inc then accum else unroll ((swap e):accum) ss

type Chain = [((Int, Int), Segment)]

chainWithSides :: Epsilon -> [(Int, Int, Dist)] -> Polygon -> Chain
chainWithSides eps is poly = zip (longestChain eps is poly) (cyclePairs poly)

cycleShifts :: [a] -> [[a]]
cycleShifts xs = [(drop i xs) ++ take i xs | i <- [0..l-1]]
    where l = length xs

allChains :: Epsilon -> [(Int, Int, Dist)] -> Polygon -> [Chain]
allChains eps is poly = filter (not . null) $ map (chainWithSides eps is) (cycleShifts poly)

-- Pick a long set of chains covering the hole
longChainCover :: Epsilon -> [(Int, Int, Dist)] -> Polygon -> Maybe Chain
longChainCover eps is poly = if null chains then Nothing else Just $ pack [] chSorted
    where subchains = filter (not . null) . concat . map inits . tails
          chains = filter (\cs -> length cs > 2) . concatMap subchains $ allChains eps is poly -- TODO
          chSorted = reverse $ sortBy (compare `on` length) chains
          chainToList :: Chain -> ([Int], [Point])
          chainToList xs = (concatMap (\((a, b), _) -> [a, b]) xs, concatMap (\(_, (p, q)) -> [p, q]) xs)
          disjoint c1 c2 = let (c1f, c1h) = chainToList c1
                               (c2f, c2h) = chainToList c2
                           in null (intersect c1f c2f) && null (intersect c1h c2h)
          pack accum [] = accum
          pack accum (c:cs) = if disjoint accum c then pack (c ++ accum) cs else pack accum cs

mapLongestChain :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> [Point]
mapLongestChain eps is xs poly =
    case chain of
      Nothing -> xs
      Just ys -> map snd . IM.toList $ foldl go ps ys
    where ps = IM.fromList $ zip [0..] xs
          chain = longChainCover eps is poly
          go :: IM.IntMap Point -> ((Int, Int), Segment) -> IM.IntMap Point
          go a ((i, j), (l, r)) = IM.insert i l $ IM.insert j r a

commonPoint :: Eq a => (a, a) -> (a, a) -> [a]
commonPoint (a, b) (c, d) = intersect [a, b] [c, d]

adjustCorners :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> [Point]
adjustCorners eps is xs poly = map snd . IM.toList $ foldl update ps cornersPoly
    where numv = length xs
          ps = IM.fromList $ zip [0..] xs
          sides = cyclePairs poly
          cornersPoly = cyclePairs sides
          matchingEdges s = map (\(i, j, _) -> (i, j)) $ filter (\(i, j, d) -> canStretch eps d s == EQ) is
          matchingCorners (s1, s2) = concat [commonPoint e1 e2 | e1 <- matchingEdges s1, e2 <- matchingEdges s2]
          update :: IM.IntMap Point -> (Segment, Segment) -> IM.IntMap Point
          update ys corner =
              let mc = matchingCorners corner
                  c = snd $ fst corner -- mc == ((_, c), (c, _))
              in foldl (\zs -> magnet zs c) ys mc
          magnet :: IM.IntMap Point -> Point -> Int -> IM.IntMap Point
          magnet ys c i =
              let p = ys IM.! i
                  cp = c .-. p
                  len = 1 + (isqrt $ dist c p)
                  p' = p .+. (cp `vdiv` len)
              in if p `elem` poly || p' `elem` poly
                 then ys
                 else IM.insert i p' ys

repelPoints :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Polygon -> [Point]
repelPoints eps is xs poly = map snd . IM.toList $ foldl go ps springs
    where ps = IM.fromList $ zip [0..] xs
          numv = length xs
          springs = [(i, j, isqrt eps + 1) | i <- [0..numv-1], j <- [i+1..numv-1]]
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
                     GT -> ys

centerOfMass :: Polygon -> Point
centerOfMass ps = (foldl (.+.) (0,0) ps) `vdiv` (length ps)

centering :: Polygon -> [Point] -> [Point]
centering poly fig = map (dv .+.) fig
    where cm = centerOfMass poly
          cf = centerOfMass fig
          dv = cm .-. cf
