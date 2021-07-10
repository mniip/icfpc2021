{-# LANGUAGE DerivingStrategies, BangPatterns, TypeApplications #-}
module Main where

import System.Environment
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import System.Random.Stateful
import System.Exit
import qualified Data.IntMap as IM

import ICFPC.Geometry hiding (isValid)
import ICFPC.Annealing
import ICFPC.JSON

main :: IO ()
main = do
  [probFile, solFile] <- getArgs
  problem <- decodeProblem <$> BSL.readFile probFile
  eSolution <- try @SomeException $ evaluate =<< decodeSolution <$> BSL.readFile solFile
  let boundary = (\(Pair x y) -> sp x y) <$> prHole problem
  let !edges = mkEdges (mkVertices $ figVertices $ prFigure problem) (figEdges $ prFigure problem)
  let !vertices = mkVertices $ case eSolution of
        Left _ -> figVertices (prFigure problem)
        Right sol -> solVertices sol
  let !eps = prEpsilon problem

  gen <- newIOGenM =<< newStdGen
  let
    go best temp vs = do
      vs' <- pickNeighbor eps boundary edges gen 64 temp vs
      let !e = energy eps boundary edges vs'
      let !sc@(validity, _, score) = (isValid eps boundary edges vs', e, dislikes boundary (IM.elems vs'))
      when (sc < best) $ do
        putStrLn $ solFile <> " New best score: " <> show sc
        BSL.writeFile solFile $ encodeSolution $ Solution $ outputVertices vs'
      when (validity == Ok && score == 0) exitSuccess
      temp' <- if temp < e / 100
        then do
          putStrLn $ solFile <> " ran out of juice: " <> show sc
          pure 300
        else pure $ temp * 0.9995
      go (min sc best) temp' vs'
  let !e = energy eps boundary edges vertices
  go (isValid eps boundary edges vertices, e, dislikes boundary (IM.elems vertices)) e vertices
