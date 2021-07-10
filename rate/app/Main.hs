{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import System.Exit
import qualified Data.IntMap as IM
import qualified Data.ByteString.Lazy as BSL

import ICFPC.JSON
import ICFPC.Geometry
import ICFPC.Annealing

main :: IO ()
main = do
  [probFile, solFile] <- getArgs
  problem <- decodeProblem <$> BSL.readFile probFile
  sol <- decodeSolution <$> BSL.readFile solFile
  let boundary = (\(Pair x y) -> sp x y) <$> prHole problem
  let !edges = mkEdges (mkVertices $ figVertices $ prFigure problem) (figEdges $ prFigure problem)
  let !vertices = mkVertices $ solVertices sol
  let !eps = prEpsilon problem
  if isValid eps boundary edges vertices == Ok
  then print $ dislikes boundary (IM.elems vertices)
  else exitFailure