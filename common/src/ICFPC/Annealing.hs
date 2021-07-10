{-# LANGUAGE DerivingStrategies, BangPatterns #-}
module ICFPC.Annealing where

import Control.Monad
import qualified Data.IntMap.Strict as IM
import System.Random.Stateful
import Control.Exception

import ICFPC.Geometry
import ICFPC.JSON
import qualified ICFPC.IntPairMap as IPM

type Edges = IPM.IntPairMap Int

sp !x !y = (x, y)

type Vertices = IM.IntMap (Int, Int)

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

-- minimal translation required to untangle the segments
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

-- multiply the inequality |d(x)/d - 1| < ... through by sqrt(d)/2 so that it's roughly linear in x and y
lengthEnergy :: Epsilon -> Segment -> Dist -> Double
lengthEnergy eps seg d =
  max 0.0 $ abs (fromIntegral d' / (sqrtD * 2) - sqrtD / 2) - fromIntegral eps * sqrtD / 2e6
  where
    !d' = distSeg seg
    !sqrtD = sqrt $ fromIntegral d

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
mkVertices xs = IM.fromList $ zip [0..] [(x, y) | Pair !x !y <- xs]

outputVertices :: Vertices -> [Pair Int]
outputVertices xs = (\(x, y) -> Pair x y) <$> IM.elems xs

mkEdges :: Vertices -> [Pair Int] -> Edges
mkEdges vs is = IPM.fromList [(u, v, d) | Pair !u !v <- is, let !d = dist (vs IM.! u) (vs IM.! v)]

mkEdgesWeighted :: [(Int, Int, Dist)] -> Edges
mkEdgesWeighted = IPM.fromList

data Validity = Ok | NotOk
  deriving stock (Eq, Ord, Show)

isValid :: Epsilon -> Polygon -> Edges -> Vertices -> Validity
isValid eps boundary es vs = if boundaryValid && edgesValid then Ok else NotOk
  where
    boundaryValid = all (\(u, v, _) -> segmentInPolygon boundary (vs IM.! u, vs IM.! v)) $ IPM.toList es
    edgesValid = all (\(u, v, d) -> canStretch eps d (vs IM.! u, vs IM.! v) == EQ) $ IPM.toList es

vertexNeighbors :: Int -> Edges -> Vertices -> [(Point, Dist)]
vertexNeighbors v es vs = map (\(v', d) -> (vs IM.! v', d)) $ IPM.neighbors es v

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
  {-
  dir <- uniformRM (0, 7) gen
  let dir' = if dir < 4 then dir else dir + 1
  let delta@(!_, !_) = (dir' `div` 3 - 1, dir' `mod` 3 - 1)
  pure $ IM.adjust (.+. delta) v vs
  -}

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

pickNeighbor :: Epsilon -> Polygon -> Edges -> IOGenM StdGen -> Int -> Double -> Vertices -> IO Vertices
pickNeighbor eps bound es gen k temp vs = do
  vss <- (vs:) <$> replicateM k (randomMutation gen eps bound es vs)
  -- boltzmann distribution
  weightedChoice gen ((\e -> exp (-e / temp)) . energy eps bound es) vss
