{-# LANGUAGE BangPatterns, ViewPatterns #-}
-- Rate a solution to a problem
--
-- ./rate <problem number> <solution.json>
-- The problem is assumed to be in ./problems/<problem number>.problem
--
-- If invalid, attempts to explain why. Doesn't support the Supeflex bonus.
module Main where

import System.Environment
import qualified Data.ByteString.Lazy as BSL

import ICFPC.JSON
import ICFPC.Problem

main :: IO ()
main = do
  [read -> probNum, solFile] <- getArgs
  spec <- readProblem probNum
  sol <- decodePose <$> BSL.readFile solFile
  case checkSolution spec sol of
    Left str -> error str
    Right score -> print score
