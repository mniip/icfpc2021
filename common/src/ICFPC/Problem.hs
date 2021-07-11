{-# LANGUAGE DerivingStrategies, BangPatterns, RecordWildCards #-}

module ICFPC.Problem where

import qualified Data.IntMap as IM
import qualified Data.ByteString.Lazy as BSL

import ICFPC.Vector
import ICFPC.Polygon
import ICFPC.JSON
import qualified ICFPC.IntPairMap as IPM

type Epsilon = Int

data ProblemSpec = ProblemSpec
  { psEdges :: IPM.IntPairMap Dist
  , psHole :: Polygon
  , psEpsilon :: Epsilon
  , psBoundingBox :: S2 -- (min, max)
  , psOriginalVertices :: IM.IntMap V2
  , psBonuses :: [BonusDescription]
  }
  deriving stock (Show)

mkProblemSpec :: Problem -> ProblemSpec
mkProblemSpec Problem{..} = ProblemSpec
  { psEdges = IPM.fromList [(u, v, dist (vertices IM.! u) (vertices IM.! v)) | (u, v) <- figEdges prFigure]
  , psHole = poly
  , psEpsilon = prEpsilon
  , psBonuses = prBonuses
  , psBoundingBox = polyBoundingBox poly
  , psOriginalVertices = vertices
  }
  where
    !vertices = IM.fromList $ zip [0..] [V2 x y | (x, y) <- figVertices prFigure]
    !poly = mkPolyCCW $ mkPolygon [V2 x y | (x, y) <- prHole]

readProblem :: Int -> IO ProblemSpec
readProblem n = mkProblemSpec . decodeProblem <$> BSL.readFile ("problems/" <> show n <> ".problem")
