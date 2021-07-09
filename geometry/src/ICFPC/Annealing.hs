{-# LANGUAGE DerivingStrategies, BangPatterns #-}
module ICFPC.Annealing where

import Control.Monad
import qualified Data.IntMap.Strict as IM
import System.Random.Stateful
import Control.Exception

import ICFPC.Geometry
import ICFPC.JSON
import qualified ICFPC.IntPairMap as IPM

type Edges = IPM.IntPairMap Integer

sp !x !y = (x, y)

type Vertices = IM.IntMap (Integer, Integer)

dot (x, y) (z, w) = x * z + y * w

-- minimal translation required to untangle the segments
overlapEnergy :: Segment -> Segment -> Double
overlapEnergy (a@(!_, !_), b@(!_, !_)) (p@(!_, !_), q@(!_, !_)) =
  if pab * qab < 0 && apq * bpq < 0
  then fromInteger (min (abs pab) (abs qab)) / lab
  else 0.0
  where
    oab = case a .-. b of (x, y) -> (-y, x)
    opq = case p .-. q of (x, y) -> (-y, x)
    !pab = oab `dot` (p .-. a)
    !qab = oab `dot` (q .-. a)
    !apq = opq `dot` (a .-. p)
    !bpq = opq `dot` (b .-. p)
    lab = sqrt $ fromInteger $ oab `dot` oab

distanceEnergy :: Point -> Segment -> Double
distanceEnergy p@(!_, !_) (a@(!_, !_), b@(!_, !_)) =
  if t >= 0 && t <= 1
  then fromInteger (abs $ oab `dot` ap) / lab
  else min lap lbp
  where
    !ab = a .-. b
    !ap = a .-. p
    !t = fromInteger (ab `dot` ap) / lab
    oab = case ab of (x, y) -> (-y, x)
    lab = sqrt $ fromInteger $ ab `dot` ab
    lap = sqrt $ fromInteger $ ap `dot` ap
    bp = b .-. p
    lbp = sqrt $ fromInteger $ bp `dot` bp

-- multiply the inequality |d(x)/d - 1| < ... through by sqrt(d)/2 so that it's roughly linear in x and y
lengthEnergy :: Epsilon -> Segment -> Dist -> Double
lengthEnergy eps seg d =
  max 0.0 $ abs (fromInteger d' / (sqrtD * 2) - sqrtD / 2) - fromInteger eps * sqrtD / 2e6
  where
    !d' = distSeg seg
    !sqrtD = sqrt $ fromInteger d

energy :: Epsilon -> Polygon -> Edges -> Vertices -> Double
energy eps boundary es vs = boundaryEnergies + lengthEnergies / 10 + scoreEnergies / 100
  where
    boundaryEnergies = sum [overlapEnergy bound (vs IM.! u, vs IM.! v) | bound <- cyclePairs boundary, (u, v, _) <- IPM.toList es]
      + sum [if pointInPolygon boundary p then 0.0 else minimum [distanceEnergy p bound | bound <- cyclePairs boundary] | p <- IM.elems vs]
    lengthEnergies = sum [lengthEnergy eps (vs IM.! u, vs IM.! v) d | (u, v, d) <- IPM.toList es]
    scoreEnergies = sqrt $ fromInteger $ sum [minimum [distSeg (b, v) | v <- IM.elems vs] | b <- boundary]

mkVertices :: [Pair Integer] -> Vertices
mkVertices xs = IM.fromList $ zip [0..] [(x, y) | Pair !x !y <- xs]

outputVertices :: Vertices -> [Pair Integer]
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

randomMutation :: IOGenM StdGen -> Vertices -> IO Vertices
randomMutation gen vs = do
  let (!maxI, _) = IM.findMax vs
  v <- uniformRM (0, maxI) gen
  dir <- uniformRM (0, 7) gen
  let dir' = if dir < 4 then dir else dir + 1
  let delta@(!_, !_) = (dir' `div` 3 - 1, dir' `mod` 3 - 1)
  pure $ IM.adjust (.+. delta) v vs

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
  vss <- (vs:) <$> replicateM k (randomMutation gen vs)
  -- boltzmann distribution
  weightedChoice gen ((\e -> exp (-e / temp)) . energy eps bound es) vss
