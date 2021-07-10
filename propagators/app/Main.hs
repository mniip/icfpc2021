{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.IORef
import System.Environment
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Control.Concurrent.Async

import ICFPC.Propagators
import ICFPC.JSON
import ICFPC.Geometry

main :: IO ()
main = do
  [probFile, solFile] <- getArgs
  problem <- decodeProblem <$> BSL.readFile probFile
  let boundary = (\(Pair x y) -> (x, y)) <$> prHole problem
  let !vertices = (\(Pair x y) -> (x, y)) <$> figVertices (prFigure problem)
  let !edges = [(u, v, distSeg (vertices !! u, vertices !! v)) | Pair u v <- figEdges $ prFigure problem]
  let !eps = prEpsilon problem
  let !numVs = length vertices

  bestRef <- newIORef Nothing

  circ <- vertexCircuit eps boundary edges numVs
  runCircuit circ
  let
    report vs =  do
      if isValid eps boundary (map (\(u, v, _) -> (u, v)) edges) vertices vs
      then do
        let !score = dislikes boundary vs
        isBest <- atomicModifyIORef' bestRef $ \mBest -> if maybe True (> score) mBest then (Just score, True) else (mBest, False)
        when isBest $ do
          putStrLn $ solFile <> ": New best: " <> show score
          BSL.writeFile solFile $ encodeSolution $ Solution [Pair x y | (x, y) <- vs]
      else putStrLn $ solFile <> ": Invalid (!?) " <> show vs
  forConcurrently_ [0..numVs-1] $ \i ->
    forConcurrently_ boundary $ \p -> do
      circ' <- cloneCircuit circ
      triggerNode circ' i (Finite $ S.singleton p)
      iterateCircuit circ' report
  putStrLn $ solFile <> ": Finished corner placements"
  circ' <- cloneCircuit circ
  masks <- viewNodes circ'
  forM_ (IM.toList masks) $ \(i, s) -> case s of
    Full -> pure () -- unreachable
    Finite s -> triggerNode circ' i $ Finite $ foldl' (flip S.delete) s boundary
  iterateCircuit circ' report
  putStrLn $ solFile <> ": Finished search"
