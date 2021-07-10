{-# LANGUAGE DerivingStrategies, TemplateHaskell #-}
module ICFPC.JSON where

import Data.Aeson
import Data.Aeson.Types hiding (Pair)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Vector hiding (length)

import ICFPC.JSON.Options

data Pair a = Pair a a
  deriving stock (Eq, Ord, Show)

instance ToJSON a => ToJSON (Pair a) where
  toJSON (Pair a b) = listValue toJSON [a, b]

instance FromJSON a => FromJSON (Pair a) where
  parseJSON = withArray "expected array" $ \arr ->
    if length arr == 2
    then Pair <$> parseJSON (arr ! 0) <*> parseJSON (arr ! 1)
    else fail "expected array of length 2"

type Coord = Int
type Idx = Int

data Figure = Figure
  { figVertices :: [Pair Int]
  , figEdges :: [Pair Idx]
  }
  deriving (Eq, Ord, Show)

data Problem = Problem
  { prHole :: [Pair Int]
  , prFigure :: Figure
  , prEpsilon :: Int
  }
  deriving (Eq, Ord, Show)

data Solution = Solution
  { solVertices :: [Pair Int]
  }
  deriving (Eq, Ord, Show)

deriveJSON jsonOptions ''Figure
deriveJSON jsonOptions ''Problem
deriveJSON jsonOptions ''Solution

decodeProblem :: BSL.ByteString -> Problem
decodeProblem = either error id . eitherDecode

encodeProblem :: Problem -> BSL.ByteString
encodeProblem = encode

decodeSolution :: BSL.ByteString -> Solution
decodeSolution = either error id . eitherDecode

encodeSolution :: Solution -> BSL.ByteString
encodeSolution = encode
