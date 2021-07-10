{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.IORef
import System.Environment
import qualified Data.ByteString.Lazy as BSL

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

  bestRef <- newIORef Nothing

  circ <- vertexCircuit eps boundary edges (length vertices)
  iterateCircuit circ $ \vs -> do
    if isValid eps boundary (map (\(u, v, _) -> (u, v)) edges) vertices vs
    then do
      let !score = dislikes boundary vs
      mBest <- readIORef bestRef
      when (maybe True (> score) mBest) $ do
        putStrLn $ solFile <> ": New best: " <> show score
        writeIORef bestRef (Just score)
        BSL.writeFile solFile $ encodeSolution $ Solution [Pair x y | (x, y) <- vs]
    else putStrLn $ solFile <> ": Invalid (!?) " <> show vs
