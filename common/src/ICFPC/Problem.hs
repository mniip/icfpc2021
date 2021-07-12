{-# LANGUAGE DerivingStrategies, BangPatterns, RecordWildCards, LambdaCase #-}
-- Utilities for a more efficient internal representation of a problem, and a validity/rating evaluator
module ICFPC.Problem where

import Control.Monad
import Data.Monoid
import Data.List (find, findIndices)
import qualified Data.IntMap as IM
import qualified Data.ByteString.Lazy as BSL

import ICFPC.Vector
import ICFPC.Polygon
import ICFPC.JSON
import qualified ICFPC.IntPairMap as IPM
import qualified ICFPC.RLE2D as R2

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

-- Check solution taking bonuses into account. Superflex not implemented.
checkSolutionWithCache :: (V2 -> Bool) -> (S2 -> Bool) -> ProblemSpec -> Pose -> Either String Int
checkSolutionWithCache vInPoly sInPoly ProblemSpec{..} Pose{..} = do
  let (Any global, First mBreak, Any wallHack) = foldMap (\case
          GlobalistUse{..} -> (Any True, mempty, mempty)
          BreakALegUse{..} -> (mempty, First $ Just buBrokenLeg, mempty)
          WallHackUse{..} -> (mempty, mempty, Any True)
        ) poseBonuses
  edges <- case mBreak of
    Nothing -> pure [(u, v, fromIntegral d) | (u, v, d) <- IPM.toList psEdges]
    Just (i, j) -> case IPM.lookup i j psEdges of
      Nothing -> Left $ "No edge to break: " <> show (i, j)
      Just d' -> pure $
        [ (i, IM.size psOriginalVertices, fromIntegral d' / 4)
        , (j, IM.size psOriginalVertices, fromIntegral d' / 4)
        ] <> [(u, v, fromIntegral d) | (u, v, d) <- IPM.toList psEdges]
  let vertices = packV2 <$> poseVertices
  case mBreak of
    Nothing -> when (length vertices /= IM.size psOriginalVertices) $
      Left $ "Expected " <> show (IM.size psOriginalVertices) <> " got " <> show (length vertices) <> " edges"
    Just _ -> when (length vertices /= IM.size psOriginalVertices + 1) $
      Left $ "Expected " <> show (IM.size psOriginalVertices) <> "+1 got " <> show (length vertices) <> " edges"
  let wallhacking = findIndices (not . vInPoly) vertices
  if wallHack
  then when (length wallhacking > 1) $
    Left $ "Multiple vertices wallhacking: " <> show wallhacking
  else when (not $ null wallhacking) $
    Left $ "Wallhacking: " <> show wallhacking
  forM_ edges $ \(u, v, _) -> do
    when (not (u `elem` wallhacking) && not (u `elem` wallhacking)) $ do
      when (not $ sInPoly $ S2V2 (vertices !! u) (vertices !! v)) $
        Left $ "Edge crosses border: " <> show (u, v)
  let epsilons =
       [ ((u, v), abs $ fromIntegral (dist (vertices !! u) (vertices !! v)) / d - 1)
       | (u, v, d) <- edges
       ]
  if global
  then do
    let allowance = fromIntegral (length edges) * fromIntegral psEpsilon / 1000000 :: Rational
    when (sum (map snd epsilons) > allowance) $
      Left $ "Sum of epsilons " <> show (sum $ map snd epsilons) <> " exceeds allowance " <> show allowance
  else case find ((> fromIntegral psEpsilon / 1000000) . snd) epsilons of
    Nothing -> pure ()
    Just ((u, v), e) -> Left $ "Edge " <> show (u, v) <> " is off by " <> show e <> " > " <> show psEpsilon
  pure $ sum [minimum [dist h v | v <- vertices] | h <- polygonVertices psHole]

checkSolution :: ProblemSpec -> Pose -> Either String Int
checkSolution spec pose = checkSolutionWithCache (`R2.member` rle) sInPoly spec pose
  where
    rle = computePolygonInternals (psHole spec)
    sInPoly (S2V2 a b) = a `R2.member` rle && b `R2.member` computePolygonVisibility (psHole spec) a
