{-# LANGUAGE BangPatterns, ViewPatterns #-}
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
