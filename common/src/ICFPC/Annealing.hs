{-# LANGUAGE DerivingStrategies, BangPatterns #-}
-- Utilities for simulated annealing
module ICFPC.Annealing where

import Control.Monad
import qualified Data.IntMap.Strict as IM
import System.Random.Stateful
import Control.Exception

import ICFPC.Geometry
import ICFPC.JSON
import qualified ICFPC.IntPairMap as IPM

type Edges = IPM.IntPairMap Int

type Vertices = IM.IntMap (Int, Int)

-- A yet another point datatype, so that energy calculations are a little bit faster
data PointNative = Point {-# UNPACK #-} !Int !Int

{-# INLINE toPointNative #-}
toPointNative :: Point -> PointNative
toPointNative = \(x, y) -> Point x y

fromPointNative :: PointNative -> Point
fromPointNative (Point x y) = (x, y)

data SegmentNative = Segment {-# UNPACK #-} !PointNative !PointNative

{-# INLINE toSegmentNative #-}
toSegmentNative :: Segment -> SegmentNative
toSegmentNative = \(a, b) -> Segment (toPointNative a) (toPointNative b)

dot :: PointNative -> PointNative -> Int
dot (Point x y) (Point z w) = x * z + y * w

(.--.) :: PointNative -> PointNative -> PointNative
Point x1 y1 .--. Point x2 y2 = Point (x1 - x2) (y1 - y2)

sideways :: PointNative -> PointNative
sideways (Point x y) = Point (-y) x

-- If two segments overlap, minimal translation required to untangle them
overlapEnergy :: SegmentNative -> SegmentNative -> Double
overlapEnergy (Segment a b) (Segment p q) =
  if pab * qab < 0 && apq * bpq < 0
  then fromIntegral (min (abs pab) (abs qab)) / lab
  else 0.0
  where
    oab = sideways $ a .--. b
    opq = sideways $ p .--. q
    !pab = oab `dot` (p .--. a)
    !qab = oab `dot` (q .--. a)
    !apq = opq `dot` (a .--. p)
    !bpq = opq `dot` (b .--. p)
    lab = sqrt $ fromIntegral $ oab `dot` oab

-- Distance from a point to a segment
distanceEnergy :: PointNative -> SegmentNative -> Double
distanceEnergy !p (Segment a b) =
  if t >= 0 && t <= 1
  then fromIntegral (abs $ oab `dot` ap) / lab
  else min lap lbp
  where
    !ab = a .--. b
    !ap = a .--. p
    !t = fromIntegral (ab `dot` ap) / lab
    oab = sideways ab
    lab = sqrt $ fromIntegral $ ab `dot` ab
    lap = sqrt $ fromIntegral $ ap `dot` ap
    bp = b .--. p
    lbp = sqrt $ fromIntegral $ bp `dot` bp

-- How "wrong" the edge length is.
-- Multiply the inequality |d(x)/d - 1| < ... through by sqrt(d)/2 so that it's roughly linear in x and y.
-- This makes edges of different length have comparable notions of wrongness
lengthEnergy :: Epsilon -> Segment -> Dist -> Double
lengthEnergy eps seg d =
  max 0.0 $ abs (fromIntegral d' / (sqrtD * 2) - sqrtD / 2) - fromIntegral eps * sqrtD / 2e6
  where
    !d' = distSeg seg
    !sqrtD = sqrt $ fromIntegral d

-- Compute the energy (lower is better) of a solution. Composed of:
-- - Large penalty for any edges crossing the boundary (gradient towards making them not intersect)
-- - Large penalty for any points outside the boundary (gradient towards closest edge)
-- (this is unideal: an edge could lie outside with both endpoints on the boundary edges)
-- - Medium penalty for any edges of incorrect length (gradient towards correct length)
-- - Small penalty based on score
--
-- This prioritizes getting correct solutions over getting good solutions, but after correctness is attained, we
-- still optimize the score, and correctness acts as a non-strict constraint.
energy :: Epsilon -> Polygon -> Edges -> Vertices -> Double
energy eps boundary es vs = boundaryEnergies + lengthEnergies / 10 + scoreEnergies / 100
  where
    boundaryEnergies = sum [overlapEnergy bound (Segment (vs' IM.! u) (vs' IM.! v)) | bound <- boundary', (u, v, _) <- IPM.toList es]
      + sum [if pointInPolygon boundary (fromPointNative p) then 0.0 else minimum [distanceEnergy p bound | bound <- boundary'] | p <- IM.elems vs']
    lengthEnergies = sum [lengthEnergy eps (vs IM.! u, vs IM.! v) d | (u, v, d) <- IPM.toList es]
    scoreEnergies = sqrt $ fromIntegral $ sum [minimum [distSeg (b, v) | v <- IM.elems vs] | b <- boundary]
    !boundary' = map toSegmentNative $ cyclePairs boundary
    !vs' = IM.map toPointNative vs

mkVertices :: [Pair Int] -> Vertices
mkVertices xs = IM.fromList $ zip [0..] [(x, y) | (!x, !y) <- xs]

outputVertices :: Vertices -> [Pair Int]
outputVertices xs = IM.elems xs

mkEdges :: Vertices -> [Pair Int] -> Edges
mkEdges vs is = IPM.fromList [(u, v, d) | (!u, !v) <- is, let !d = dist (vs IM.! u) (vs IM.! v)]

mkEdgesWeighted :: [(Int, Int, Dist)] -> Edges
mkEdgesWeighted = IPM.fromList

data Validity = Ok | NotOk
  deriving stock (Eq, Ord, Show)

-- A yet another implementation of validity
isValid :: Epsilon -> Polygon -> Edges -> Vertices -> Validity
isValid eps boundary es vs = if boundaryValid && edgesValid then Ok else NotOk
  where
    boundaryValid = all (\(u, v, _) -> segmentInPolygon boundary (vs IM.! u, vs IM.! v)) $ IPM.toList es
    edgesValid = all (\(u, v, d) -> canStretch eps d (vs IM.! u, vs IM.! v) == EQ) $ IPM.toList es

vertexNeighbors :: Int -> Edges -> Vertices -> [(Point, Dist)]
vertexNeighbors v es vs = map (\(v', d) -> (vs IM.! v', d)) $ IPM.neighbors es v

-- Randomly jiggle the vertices
randomMutation :: IOGenM StdGen -> Epsilon -> Polygon -> Edges -> Vertices -> IO Vertices
randomMutation gen eps poly es vs = do
  let (!maxI, _) = IM.findMax vs
  v <- uniformRM (0, maxI) gen
  let vnbs = vertexNeighbors v es vs
      vnbs' = if null vnbs then [] else tail vnbs -- relax restrictions a bit
      nbrs = (vs IM.! v) : (allowedPositions poly (max eps 1000000) vnbs)
      lnbrs = length nbrs
  i <- uniformRM (0, lnbrs-1) gen
  pure $ IM.insert v (nbrs !! i) vs

-- Try sending an edge to a polygon side
randomEdgeMutation :: IOGenM StdGen -> Epsilon -> Polygon -> Edges -> Vertices -> IO Vertices
randomEdgeMutation gen eps poly es vs = do
  let esList = IPM.toList es
  k <- uniformRM (0, length esList - 1) gen
  let (i, j, d) = esList !! k -- a random edge
      polygonSides = cyclePairs poly
      hosts = filter ((==EQ) . canStretch eps d) polygonSides
  if null hosts
  then return vs
  else do
      s <- uniformRM (0, length hosts - 1) gen
      let h = hosts !! s
      return . IM.insert i (fst h) $ IM.insert j (snd h) vs

-- Try sending a point to a polygon vertex
randomPointMutation :: IOGenM StdGen -> Epsilon -> Polygon -> Edges -> Vertices -> IO Vertices
randomPointMutation gen eps poly es vs = do
  k <- uniformRM (0, length poly - 1) gen
  let v = poly !! k
      vlen = IM.size vs
  i <- uniformRM (0, vlen-1) gen
  return $ IM.insert i v vs

-- Try shifting all points
allShifts :: Vertices -> [Vertices]
allShifts vs = [fmap (.+. (dx, dy)) vs | dx <- [-1..1], dy <- [-1..1], not (dx == 0 && dy == 0)]

randomInit :: IOGenM StdGen -> Polygon -> Int -> IO [Pair Int]
randomInit gen poly numv = replicateM numv randomVertex
    where polyl = length poly
          randomVertex = do
              i <- uniformRM (0, polyl-1) gen
              return $ poly !! i

-- Pick a random value with the probability proportional to the return of the weight function
weightedChoice :: IOGenM StdGen -> (a -> Double) -> [a] -> IO a
weightedChoice gen weight xs = do
  let !totalWeight = sum weights
  choice <- uniformRM (0, totalWeight) gen
  evaluate $ go choice weights xs
  where
    weights = map weight xs
    go !ch (w:ws) (x:xs)
      | ch - w <= 0 = x
      | otherwise = go (ch - w) ws xs

twoFold :: (IOGenM StdGen -> Epsilon -> Polygon -> Edges -> Vertices -> IO Vertices) -> 
            IOGenM StdGen -> Epsilon -> Polygon -> Edges -> Vertices -> IO Vertices
twoFold fun gen eps poly es vs = do
    vs' <- fun gen eps poly es vs
    vs'' <- fun gen eps poly es vs'
    return vs''

-- Pick a random "neighboring" state
pickNeighbor :: Epsilon -> Polygon -> Edges -> IOGenM StdGen -> Int -> Double -> Vertices -> IO Vertices
pickNeighbor eps bound es gen k temp vs = do
  let is = IPM.toList es
      xs = map snd $ IM.toAscList vs
      blowUp = mkVertices $ putPointsAway eps is xs bound
      adjusted = mkVertices $ adjustPoints eps is xs []
      shifted = allShifts vs
  annula <- replicateM k (twoFold randomMutation gen eps bound es vs)
  sides <- replicateM k (randomEdgeMutation gen eps bound es vs)
  points <- replicateM k (randomPointMutation gen eps bound es vs)
  let vss = [adjusted, {- blowUp, -} vs] ++ shifted ++ annula ++ sides ++ points
  -- boltzmann distribution
  weightedChoice gen ((\e -> exp (-e / temp)) . energy eps bound es) vss
