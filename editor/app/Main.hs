{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import Data.IORef
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact
import qualified Data.ByteString.Lazy as BSL
import Control.Arrow

import ICFPC.JSON

data World = World
  { wModifyViewPort :: (ViewPort -> IO ViewPort) -> IO ()
  , wHole :: [(Integer, Integer)]
  , wGrid :: ((Integer, Integer), (Integer, Integer))
  , wEdges :: [(Int, Int)]
  , wVertices :: [(Integer, Integer)]
  , wMouseCoords :: (Integer, Integer)
  , wDragging :: Maybe Int
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
  interactIO
    FullScreen
    black
    World
      { wModifyViewPort = \vp -> do
          modVP <- readIORef vpRef
          modVP vp
      , wHole = hole
      , wGrid = boundingGrid hole
      , wEdges = fromPair <$> figEdges (prFigure problem)
      , wVertices = fromIntegerPointList $ fromPair <$> figVertices (prFigure problem)
      , wMouseCoords = (0, 0)
      , wDragging = Nothing
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
  [ Color (greyN 0.5) $ gridPicture (wGrid world)
  , Color red $ holePicture (wHole world)
  , Color green $ figurePicture (wEdges world) (wVertices world)
  , case fromIntegerPoint $ wMouseCoords world of (x, y) -> Color blue $ Translate x y $ ThickCircle 0.25 0.5
  ]

gridPicture :: ((Integer, Integer), (Integer, Integer)) -> Picture
gridPicture ((minX, minY), (maxX, maxY)) = Pictures $
  [ Line $ fromIntegerPointList [(x, minY), (x, maxY)] | x <- [minX .. maxX]] <>
  [ Line $ fromIntegerPointList [(minX, y), (maxX, y)] | y <- [minY .. maxY]]

figurePicture :: [(Int, Int)] -> [(Integer, Integer)] -> Picture
figurePicture is xs = Pictures $
  [Line $ fromIntegerPointList [xs !! u, xs !! v] | (u, v) <- is] <>
  [Translate x y $ ThickCircle 0.25 0.5 | (x, y) <- fromIntegerPointList xs]

holePicture :: [(Integer, Integer)] -> Picture
holePicture xs = Pictures $
  (Line $ fromIntegerPointList $ xs <> take 1 xs) :
  [Translate x y $ ThickCircle 0.125 0.25 | (x, y) <- fromIntegerPointList xs]

cursorPicture :: (Integer, Integer) -> Picture
cursorPicture _ = Blank

fromIntegerPoint :: Num a => (Integer, Integer) -> (a, a)
fromIntegerPoint = fromInteger *** fromInteger

fromIntegerPointList :: Num a => [(Integer, Integer)] -> [(a, a)]
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
