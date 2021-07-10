{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import System.Exit
import qualified Data.IntMap as IM
import qualified Data.ByteString.Lazy as BSL

import ICFPC.JSON
import ICFPC.Geometry hiding (isValid)
import ICFPC.Annealing

main :: IO ()
main = do
  [probFile, solFile] <- getArgs
  problem <- decodeProblem <$> BSL.readFile probFile
  sol <- decodePose <$> BSL.readFile solFile
  let boundary = prHole problem
  let !edges = mkEdges (mkVertices $ figVertices $ prFigure problem) (figEdges $ prFigure problem)
  let !vertices = mkVertices $ poseVertices sol
  let !eps = prEpsilon problem
  if isValid eps boundary edges vertices == Ok
  then print $ dislikes boundary (IM.elems vertices)
  else exitFailure
