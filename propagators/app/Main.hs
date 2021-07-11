{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Main where

import Control.Monad
import Data.IORef
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Control.Concurrent.Async
import Control.Concurrent

import ICFPC.Propagators
import ICFPC.JSON
import ICFPC.Polygon
import ICFPC.Problem
import ICFPC.Vector

import qualified ICFPC.RLE2D as R2

main :: IO ()
main = do
  [read -> probNumber, solFile] <- getArgs
  spec <- readProblem probNumber

  bestRef <- newIORef Nothing

  circ <- vertexCircuit spec
  runCircuit circ

  putStrLn $ solFile <> ": Init"

  let
    report vs = case checkSolution spec $ Pose (unpackV2 <$> vs) [] of
        Left str -> putStrLn $ "Invalid solution: " <> str <> " : " <> show vs
        Right score -> do
          isBest <- atomicModifyIORef' bestRef $ \mBest -> if maybe True (> score) mBest then (Just score, True) else (mBest, False)
          when isBest $ do
            putStrLn $ solFile <> ": New best: " <> show score
            BSL.writeFile solFile $ encodePose $ Pose (unpackV2 <$> vs) []
            when (score == 0) $ do
              putStrLn $ solFile <> ": Exiting early"
              exitSuccess
    numVs = IM.size $ psOriginalVertices spec

  concurrently_
    (do
    forConcurrently_ (polygonVertices $ psHole spec) $ \p ->
      forM_ [0..numVs-1] $ \i -> do
        circ' <- cloneCircuit circ
        triggerNode circ' i (Finite $ R2.singleton p)
        iterateCircuit circ' report
    putStrLn $ solFile <> ": Finished corner placements"
    )
    (do
      circ' <- cloneCircuit circ
      masks <- viewNodes circ'
      forM_ (IM.toList masks) $ \(i, s) -> case s of
        Full -> pure () -- unreachable
        Finite s -> triggerNode circ' i $ Finite $ foldl' (flip R2.delete) s (polygonVertices $ psHole spec)
      iterateCircuit circ' report
      putStrLn $ solFile <> ": Finished non-corner placements"
    )
  putStrLn $ solFile <> ": Finished search"
