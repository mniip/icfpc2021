{-# LANGUAGE DerivingStrategies, RecordWildCards, OverloadedStrings, LambdaCase #-}
module ICFPC.JSON where

import Data.Aeson
import Data.Aeson.Types hiding (Pair)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Vector hiding (length, null)

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
type ProblemId = Int

data Figure = Figure
  { figVertices :: [Pair Coord]
  , figEdges :: [Pair Idx]
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON Figure where
  toJSON Figure{..} = object
    [ "vertices" .= listValue toJSONPair figVertices
    , "edges" .= listValue toJSONPair figEdges
    ]

instance FromJSON Figure where
  parseJSON = withObject "Figure" $ \obj -> Figure
    <$> explicitParseField (listParser parseJSONPair) obj "vertices"
    <*> explicitParseField (listParser parseJSONPair) obj "edges"

data BonusType = Globalist | BreakALeg | WallHack
  deriving stock (Eq, Ord, Show)

instance ToJSON BonusType where
  toJSON Globalist = String "GLOBALIST"
  toJSON BreakALeg = String "BREAK_A_LEG"
  toJSON WallHack = String "WALLHACK"

instance FromJSON BonusType where
  parseJSON (String "GLOBALIST") = pure Globalist
  parseJSON (String "BREAK_A_LEG") = pure BreakALeg
  parseJSON (String "WALLHACK") = pure WallHack
  parseJSON _ = fail "BonusType"

data BonusDescription = BonusDescription
  { bdPosition :: Pair Coord
  , bdBonus :: BonusType
  , bdTargetProblem :: ProblemId
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON BonusDescription where
  toJSON BonusDescription{..} = object
    [ "position" .= toJSONPair bdPosition
    , "bonus" .= bdBonus
    , "problem" .= bdTargetProblem
    ]

instance FromJSON BonusDescription where
  parseJSON = withObject "BonusDescription" $ \obj -> BonusDescription
    <$> explicitParseField parseJSONPair obj "position"
    <*> obj .: "bonus"
    <*> obj .: "problem"

data Problem = Problem
  { prHole :: [Pair Coord]
  , prFigure :: Figure
  , prEpsilon :: Int
  , prBonuses :: [BonusDescription]
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON Problem where
  toJSON Problem{..} = object $
    [ "hole" .= listValue toJSONPair prHole
    , "figure" .= prFigure
    , "epsilon" .= prEpsilon
    ] <>
    [ "bonuses" .= prBonuses | not $ null prBonuses
    ]

instance FromJSON Problem where
  parseJSON = withObject "Problem" $ \obj -> Problem
    <$> explicitParseField (listParser parseJSONPair) obj "hole"
    <*> obj .: "figure"
    <*> obj .: "epsilon"
    <*> (fromMaybe [] <$> obj .:? "bonuses")

data BonusUse
  = GlobalistUse
  { buSourceProblem :: ProblemId
  }
  | BreakALegUse
  { buSourceProblem :: ProblemId
  , buBrokenLeg :: Pair Idx
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON BonusUse where
  toJSON GlobalistUse{..} = object
    [ "bonus" .= Globalist
    , "problem" .= buSourceProblem
    ]
  toJSON BreakALegUse{..} = object
    [ "bonus" .= BreakALeg
    , "problem" .= buSourceProblem
    , "edge" .= buBrokenLeg
    ]

instance FromJSON BonusUse where
  parseJSON = withObject "BonusUse" $ \obj -> obj .: "bonus" >>= \case
    Globalist -> GlobalistUse
      <$> obj .: "problem"
    BreakALeg -> BreakALegUse
      <$> obj .: "problem"
      <*> explicitParseField parseJSONPair obj "edge"

data Pose = Pose
  { poseVertices :: [Pair Coord]
  , poseBonuses :: [BonusUse]
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON Pose where
  toJSON Pose{..} = object $
    [ "vertices" .= listValue toJSONPair poseVertices
    ] <>
    [ "bonuses" .= poseBonuses | not $ null poseBonuses
    ]

instance FromJSON Pose where
  parseJSON = withObject "Pose" $ \obj -> Pose
    <$> explicitParseField (listParser parseJSONPair) obj "vertices"
    <*> (fromMaybe [] <$> obj .:? "bonuses")

decodeProblem :: BSL.ByteString -> Problem
decodeProblem = either error id . eitherDecode

encodeProblem :: Problem -> BSL.ByteString
encodeProblem = encode

decodePose :: BSL.ByteString -> Pose
decodePose = either error id . eitherDecode

encodePose :: Pose -> BSL.ByteString
encodePose = encode
