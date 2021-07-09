{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import Data.IORef
import Data.List
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Picture hiding (Point)
import Graphics.Gloss.Interface.IO.Interact hiding (Point)
import qualified Data.ByteString.Lazy as BSL
import Control.Arrow

import ICFPC.JSON
import ICFPC.Geometry

data World = World
  { wModifyViewPort :: (ViewPort -> IO ViewPort) -> IO ()
  , wHole :: Polygon
  , wGrid :: (Point, Point)
  , wEdges :: [(Int, Int, Dist)]
  , wVertices :: [Point]
  , wMouseCoords :: Point
  , wDragging :: Maybe Int
  , wEpsilon :: Integer
  }

main :: IO ()
main = do
  [probFile] <- getArgs
  vpRef <- newIORef $ error "no viewport"
  problem <- decodeProblem <$> BSL.readFile probFile
  let hole = fromPair <$> prHole problem
  let initViewPort ctrl = do
        writeIORef vpRef (controllerModifyViewPort ctrl)
        controllerModifyViewPort ctrl $ \_ -> pure $ boundingViewPort hole
  let vertices = fromIntegerPointList $ fromPair <$> figVertices (prFigure problem)
  interactIO
    FullScreen
    black
    World
      { wModifyViewPort = \vp -> do
          modVP <- readIORef vpRef
          modVP vp
      , wHole = hole
      , wGrid = boundingGrid hole
      , wEdges = (\(Pair u v) -> (u, v, dist (vertices !! u) (vertices !! v))) <$> figEdges (prFigure problem)
      , wVertices = vertices
      , wMouseCoords = (0, 0)
      , wDragging = Nothing
      , wEpsilon = prEpsilon problem
      }
    worldPicture
    onEvent
    initViewPort

onEvent :: Event -> World -> IO World
onEvent (EventMotion coords) world = do
  vpRef <- newIORef undefined
  wModifyViewPort world $ \vp -> writeIORef vpRef vp >> pure vp
  vp <- readIORef vpRef
  let newCoords = round *** round $ invertViewPort vp coords
  let world' = world { wMouseCoords = newCoords }
  case wDragging world' of
    Nothing -> pure world'
    Just i -> pure world'
      { wVertices = setNth i newCoords $ wVertices world' }
onEvent (EventKey (MouseButton LeftButton) Down _ coords) world = do
  case elemIndex (wMouseCoords world) (wVertices world) of
    Nothing -> pure world
    Just i -> pure world { wDragging = Just i }
onEvent (EventKey (MouseButton LeftButton) Up _ coords) world = do
  pure world { wDragging = Nothing }
onEvent (EventKey (MouseButton WheelUp) Down _ coords) world = do
  wModifyViewPort world $ \vp -> do
    let oldScale = viewPortScale vp
    let newScale = oldScale * 1.2
    pure vp
      { viewPortScale = newScale
      , viewPortTranslate = viewPortTranslate vp P.+ (1 / newScale P.* coords) P.- (1 / oldScale P.* coords)
      }
  pure world
onEvent (EventKey (MouseButton WheelDown) Down _ coords) world = do
  wModifyViewPort world $ \vp -> do
    let oldScale = viewPortScale vp
    let newScale = oldScale / 1.2
    pure vp
      { viewPortScale = newScale
      , viewPortTranslate = viewPortTranslate vp P.+ (1 / newScale P.* coords) P.- (1 / oldScale P.* coords)
      }
  pure world
onEvent event world = pure world

worldPicture :: World -> IO Picture
worldPicture world = pure $ Pictures
  [ Color (greyN 0.25) $ gridPicture (wGrid world)
  , Color red $ holePicture (wHole world)
  , boundariesPicture (wEpsilon world) (wDragging world) (wEdges world) (wVertices world)
  , figurePicture (wEpsilon world) (wEdges world) (wVertices world)
  , cursorPicture $ wMouseCoords world
  ]

gridPicture :: (Point, Point) -> Picture
gridPicture ((minX, minY), (maxX, maxY)) = Pictures $
  [ Line $ fromIntegerPointList [(x, minY), (x, maxY)] | x <- [minX .. maxX]] <>
  [ Line $ fromIntegerPointList [(minX, y), (maxX, y)] | y <- [minY .. maxY]]

figurePicture :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> Picture
figurePicture eps is xs = Pictures $
  [ Color (stretchColor $ canStretch eps origD (xs !! u, xs !! v)) $ Line $
    fromIntegerPointList [xs !! u, xs !! v]
  | (u, v, origD) <- is ] <>
  [Color green $ Translate x y $ ThickCircle 0.25 0.5 | (x, y) <- fromIntegerPointList xs]
  where
    stretchColor LT = yellow
    stretchColor GT = cyan
    stretchColor EQ = green

boundariesPicture :: Epsilon -> Maybe Int -> [(Int, Int, Dist)] -> [Point] -> Picture
boundariesPicture _ Nothing _ _ = Blank
boundariesPicture eps (Just i) is xs = Pictures
  [ Color (withAlpha 0.3 red) $ Translate x y $ ThickCircle ((sqrt minD + sqrt maxD) / 2) (sqrt maxD - sqrt minD)
  | (j, origD) <- [(j, d) | (i', j, d) <- is, i' == i] <> [(j, d) | (j, i', d) <- is, i' == i]
  , let (x, y) = fromIntegerPoint $ xs !! j
  , let minD = max 0 $ fromInteger origD * (1 - fromInteger eps / 1000000)
  , let maxD = fromInteger origD * (1 + fromInteger eps / 1000000)
  ]

holePicture :: Polygon -> Picture
holePicture xs = Pictures $
  (Line $ fromIntegerPointList $ xs <> take 1 xs) :
  [Translate x y $ ThickCircle 0.125 0.25 | (x, y) <- fromIntegerPointList xs]

cursorPicture :: Point -> Picture
cursorPicture coords = case fromIntegerPoint coords of
  (x, y) -> Color blue $ Translate x y $ ThickCircle 0.25 0.5

fromIntegerPoint :: Num a => Point -> (a, a)
fromIntegerPoint = fromInteger *** fromInteger

fromIntegerPointList :: Num a => [Point] -> [(a, a)]
fromIntegerPointList = map fromIntegerPoint

fromPair :: Pair a -> (a, a)
fromPair (Pair x y) = (x, y)

setNth :: Int -> a -> [a] -> [a]
setNth n x = go n
  where
    go !_ [] = []
    go !0 (_:ys) = x:ys
    go !n (y:ys) = y:go (n-1) ys

boundingGrid :: [(Integer, Integer)] -> ((Integer, Integer), (Integer, Integer))
boundingGrid xs = (minimum *** minimum) &&& (maximum *** maximum) $ unzip xs

boundingViewPort :: [(Integer, Integer)] -> ViewPort
boundingViewPort xs = case fromIntegerPoint *** fromIntegerPoint $ boundingGrid xs of
  ((minX, minY), (maxX, maxY)) -> ViewPort
    { viewPortTranslate = (- (minX + maxX) / 2, - (minY + maxY) / 2)
    , viewPortScale = min (1920 / (1 + maxX - minX)) (1080 / (1 + maxY - minY))
    , viewPortRotate = 0
    }
