{-# LANGUAGE DerivingStrategies, TemplateHaskell, RecordWildCards, OverloadedStrings #-}
module ICFPC.JSON where

import Data.Aeson
import Data.Aeson.Types hiding (Pair)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Vector hiding (length)

type Pair a = (a, a)

toJSONPair :: ToJSON a => Pair a -> Value
toJSONPair (a, b) = listValue toJSON [a, b]

parseJSONPair :: FromJSON a => Value -> Parser (a, a)
parseJSONPair = withArray "Pair" $ \arr ->
    if length arr == 2
    then (,) <$> parseJSON (arr ! 0) <*> parseJSON (arr ! 1)
    else fail "expected array of length 2"

type Coord = Int
type Idx = Int

data Figure = Figure
  { figVertices :: [Pair Coord]
  , figEdges :: [Pair Idx]
  }
  deriving (Eq, Ord, Show)

instance ToJSON Figure where
  toJSON Figure{..} = object
    [ "vertices" .= listValue toJSONPair figVertices
    , "edges" .= listValue toJSONPair figEdges
    ]

instance FromJSON Figure where
  parseJSON = withObject "Figure" $ \obj -> Figure
    <$> explicitParseField (listParser parseJSONPair) obj "vertices"
    <*> explicitParseField (listParser parseJSONPair) obj "edges"

data Problem = Problem
  { prHole :: [Pair Coord]
  , prFigure :: Figure
  , prEpsilon :: Int
  }
  deriving (Eq, Ord, Show)

instance ToJSON Problem where
  toJSON Problem{..} = object
    [ "hole" .= listValue toJSONPair prHole
    , "figure" .= prFigure
    , "epsilon" .= prEpsilon
    ]

instance FromJSON Problem where
  parseJSON = withObject "Problem" $ \obj -> Problem
    <$> explicitParseField (listParser parseJSONPair) obj "hole"
    <*> obj .: "figure"
    <*> obj .: "epsilon"

data Solution = Solution
  { solVertices :: [Pair Coord]
  }
  deriving (Eq, Ord, Show)

instance ToJSON Solution where
  toJSON Solution{..} = object
    [ "vertices" .= listValue toJSONPair solVertices
    ]

instance FromJSON Solution where
  parseJSON = withObject "Solution" $ \obj -> Solution
    <$> explicitParseField (listParser parseJSONPair) obj "vertices"

decodeProblem :: BSL.ByteString -> Problem
decodeProblem = either error id . eitherDecode

encodeProblem :: Problem -> BSL.ByteString
encodeProblem = encode

decodeSolution :: BSL.ByteString -> Solution
decodeSolution = either error id . eitherDecode

encodeSolution :: Solution -> BSL.ByteString
encodeSolution = encode
