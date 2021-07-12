{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Main where

import Control.Monad
import Control.Monad.Fix
import Data.IORef
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Control.Concurrent.Async
import Control.Concurrent
import qualified Data.Array as A
import Data.Maybe
import Data.List.Split

import ICFPC.Propagators
import ICFPC.JSON
import ICFPC.Polygon
import ICFPC.Problem
import ICFPC.Vector
import ICFPC.Geometry (stretchAnnulus)
import qualified ICFPC.RLE2D as R2
import qualified ICFPC.IntPairMap as IPM

admissibleRing :: Epsilon -> Dist -> R2.RLE2D
admissibleRing eps d = R2.fromList $ map packV2 $ stretchAnnulus eps d

vertexCircuit :: ProblemSpec -> IO (V2 -> Bool, S2 -> Bool, CircuitState ZSet)
vertexCircuit !spec = do
  circ <- newCircuitState
  let !insides = computePolygonInternals $ psHole spec
  let S2 minX minY maxX maxY = psBoundingBox spec
  let bounds = ((minX, minY), (maxX, maxY))
  let !validDeltas = A.listArray bounds
        [ R2.shift (V2 (-x) (-y)) $ computePolygonVisibility (psHole spec) (V2 x y)
        | x <- [minX..maxX], y <- [minY..maxY]
        ]
  let validTargets (V2 x y)
        | A.inRange bounds (x, y) = validDeltas A.! (x, y)
        | otherwise = R2.empty
  let numVs = length $ psOriginalVertices spec
  forM_ [0 .. numVs - 1] $ \i -> do
    let !neighbors =
          [ (j, admDelta)
          | (j, dist) <- IPM.neighbors (psEdges spec) i
          , let !admDelta = admissibleRing (psEpsilon spec) dist
          ]
    addNode circ Full $ \_ trigger old intersected new -> do
      if old `isZSubset` intersected then pure ()
      else case new of
        Full -> pure () -- unreachable
        Finite new' -> do
          when (R2.size new' < 1000) $ do
            forM_ neighbors $ \(j, admDelta) -> do
              let !admissible = R2.unions
                    [ R2.shift pos $ admDelta `R2.intersection` validTargets pos | pos <- R2.toList new' ]
              trigger j $ Finite admissible
  forM_ [0 .. numVs - 1] $ \i -> triggerNode circ i $ Finite insides
  pure ((`R2.member` insides), \(S2V2 a b) -> (b .-. a) `R2.member` validTargets a, circ)

edgeVertexCircuit :: ProblemSpec -> IO (V2 -> Bool, S2 -> Bool, CircuitState ZSet, IM.IntMap Int, IPM.IntPairMap Int)
edgeVertexCircuit !spec = do
  circ <- newCircuitState
  let !insides = computePolygonInternals $ psHole spec
  let S2 minX minY maxX maxY = psBoundingBox spec
  let bounds = ((minX, minY), (maxX, maxY))
  let !validDeltas = A.listArray bounds
        [ R2.shift (V2 (-x) (-y)) $ computePolygonVisibility (psHole spec) (V2 x y)
        | x <- [minX..maxX], y <- [minY..maxY]
        ]
  let validTargets (V2 x y)
        | A.inRange bounds (x, y) = validDeltas A.! (x, y)
        | otherwise = R2.empty
  let numVs = length $ psOriginalVertices spec
  (vertMap, edgeMap) <- mfix $ \ ~(vertMap, edgeMap) -> do
    vertIdx <- forM [0 .. numVs - 1] $ \i -> do
      let !neighbors = [(j, vertMap IM.! j, edgeMap IPM.! (i, j), i > j) | (j, _) <- IPM.neighbors (psEdges spec) i]
      iIdx <- addNode circ Full $ \states trigger old intersected new -> do
        if old `isZSubset` intersected then pure ()
        else case new of
          Full -> pure ()
          Finite new' -> do
            when (R2.size new' < 1000) $ do
              forM_ neighbors $ \(j, jIdx, ijIdx, inverted) -> do
                case (states IM.! jIdx, states IM.! ijIdx) of
                  (Finite jVals, Finite ijVals) -> do
                    let !newJVals = R2.unions
                          [ R2.shift iPos $ validTargets iPos `R2.intersection` (if inverted then R2.reflect (V2 0 0) else id) ijVals
                          | iPos <- R2.toList new'
                          ]
                    trigger jIdx $ Finite newJVals
                    let !newIJVals = R2.unions
                          [ (if inverted then R2.reflect iPos else R2.shift (negateV2 iPos)) jVals
                          | iPos <- R2.toList new'
                          ]
                    trigger ijIdx $ Finite newIJVals
                  _ -> pure ()
      triggerNode circ iIdx $ Finite insides
      pure (i, iIdx)
    edgeIdx <- forM (IPM.toList $ psEdges spec) $ \(u, v, dist) -> do
      let (!i, !j) = (min u v, max u v)
      let ~iIdx = vertMap IM.! i
      let ~jIdx = vertMap IM.! j
      let !annulus = admissibleRing (psEpsilon spec) dist
      ijIdx <- addNode circ (Finite annulus) $ \states trigger old intersected new -> do
        let !triangles =
              [ (k, edgeMap IPM.! (i, k), i > k, edgeMap IPM.! (j, k), j > k)
              | (k, _) <- IPM.neighbors (psEdges spec) i
              , (j, k) `IPM.member` psEdges spec
              ]
        if old `isZSubset` intersected then pure ()
        else case new of
          Full -> pure ()
          Finite new' -> do
            when (R2.size new' < 1000) $ do
              case (states IM.! iIdx, states IM.! jIdx) of
                (Finite iVals, Finite jVals) -> do
                  -- if we were triggered by an edge update, need to trigger at least one vertex
                  let !newJVals = R2.unions
                        [ R2.shift iPos new' `R2.intersection` jVals
                        | iPos <- R2.toList iVals
                        ]
                  trigger jIdx $ Finite newJVals
                  forM_ triangles $ \(k, ikIdx, ikInverted, jkIdx, jkInverted) -> do
                    let Finite !ikVals = states IM.! ikIdx
                    let Finite !jkVals = states IM.! jkIdx
                    if ikInverted
                    then do
                      let ls =
                            [ (ij, ikVals `R2.intersection` (if jkInverted then R2.shift (negateV2 ij) else R2.reflect (negateV2 ij)) jkVals)
                            | ij <- R2.toList new'
                            ]
                      let !newIKVals = R2.unions [l | (_, l) <- ls]
                      let !newJKVals = R2.unions [(if jkInverted then R2.shift ij else R2.reflect (negateV2 ij)) l | (ij, l) <- ls]
                      trigger ikIdx $ Finite newIKVals
                      trigger jkIdx $ Finite newJKVals
                    else do
                      let ls =
                            [ (ij, ikVals `R2.intersection` (if jkInverted then R2.reflect ij else R2.shift ij) jkVals)
                            | ij <- R2.toList new'
                            ]
                      let !newIKVals = R2.unions [l | (_, l) <- ls]
                      let !newJKVals = R2.unions [(if jkInverted then R2.reflect ij else R2.shift (negateV2 ij)) l | (ij, l) <- ls]
                      trigger ikIdx $ Finite newIKVals
                      trigger jkIdx $ Finite newJKVals
                _ -> pure ()
      triggerNode circ ijIdx $ Finite annulus
      pure (i, j, ijIdx)
    pure (IM.fromList vertIdx, IPM.fromList edgeIdx)
  pure ((`R2.member` insides), \(S2V2 a b) -> (b .-. a) `R2.member` validTargets a, circ, vertMap, edgeMap)

main :: IO ()
main = do
  [read -> probNumber, solFile] <- getArgs
  spec <- readProblem probNumber

  bestRef <- newIORef Nothing

  let say xs = putStrLn $ solFile <> ": " <> xs

  (vInPoly, sInPoly, circ, vMap, eMap) <- edgeVertexCircuit spec
  say "Created circuit"

  let
    report vs = do
      let pose = Pose (unpackV2 . (vs IM.!) <$> IM.elems vMap) []
      case checkSolutionWithCache vInPoly sInPoly spec pose of
        Left str -> say $ "Invalid solution: " <> str <> " : " <> show pose
        Right score -> do
          isBest <- atomicModifyIORef' bestRef $ \mBest -> if maybe True (> score) mBest then (Just score, True) else (mBest, False)
          when isBest $ do
            say $ "New best: " <> show score
            BSL.writeFile solFile $ encodePose pose
            when (score == 0) $ do
              say "Exiting early"
              exitSuccess

  runCircuit circ
  say "Initialized circuit"

  let nthread = 16
  circLists <- distribute nthread <$> partitionCircuit nthread circ
  forConcurrently_ circLists $ \circList -> do
    say $ "Thread for " <> show (length circList) <> " branches"
    forM_ circList $ \cic -> do
      iterateCircuit circ report
    say "Thread finished"
  say "Done"
  where
    distribute n xs = chunksOf chunk xs
      where !chunk = -((-length xs) `div` n)
