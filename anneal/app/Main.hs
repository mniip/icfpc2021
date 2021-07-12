{-# LANGUAGE DerivingStrategies, BangPatterns, TypeApplications #-}
-- Run simulated annealing
--
-- ./anneal <problem.json> <solution.json>
--
-- We try to minimize the score, and the constraints of vailidity are applied as penalties with good gradient properties
-- so that a solution can temporarily jump out of a validity region to possibly land in another.
-- We decrease temperature slowly and exponentially. If a solution appears stuck at a local minimum and we reach very
-- low temperatures, we spontaneously reheat.
module Main where

import System.Environment
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import System.Random.Stateful
import System.Exit
import qualified Data.IntMap as IM
import qualified ICFPC.IntPairMap as IPM

import ICFPC.Geometry hiding (isValid)
import ICFPC.Annealing
import ICFPC.JSON

main :: IO ()
main = do
  [probFile, solFile] <- getArgs
  problem <- decodeProblem <$> BSL.readFile probFile
  eSolution <- try @SomeException $ evaluate =<< decodePose <$> BSL.readFile solFile
  let boundary = prHole problem
  let !edges = mkEdges (mkVertices $ figVertices $ prFigure problem) (figEdges $ prFigure problem)
  let !vertices = mkVertices $ case eSolution of
        Left _ -> centering boundary (figVertices (prFigure problem))
        Right sol -> poseVertices sol
  let !eps = prEpsilon problem

  gen <- newIOGenM =<< newStdGen
  let
    go best temp vs = do
      vs_raw <- pickNeighbor eps boundary edges gen 5 temp vs
      let vs' = IM.fromList . zip [0..] $ improvePoints eps (IPM.toList edges) (map snd $ IM.toAscList vs_raw) boundary
      let !e = energy eps boundary edges vs'
      let !sc@(validity, _, score) = (isValid eps boundary edges vs', e, dislikes boundary (IM.elems vs'))
      when (sc < best) $ do
        putStrLn $ solFile <> " New best score: " <> show sc
        BSL.writeFile solFile $ encodePose $ Pose (outputVertices vs') []
      when (validity == Ok && score == 0) exitSuccess
      temp' <- if temp < e / 100
        then do
          putStrLn $ solFile <> " ran out of juice: " <> show sc
          pure 300
        else pure $ temp * 0.9995
      go (min sc best) temp' vs'
  let !e = energy eps boundary edges vertices
  go (isValid eps boundary edges vertices, e, dislikes boundary (IM.elems vertices)) e vertices
