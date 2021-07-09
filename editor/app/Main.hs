module Main where

import System.Environment
import Data.IORef
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
  }

main :: IO ()
main = do
  [probFile] <- getArgs
  vpRef <- newIORef $ error "no viewport"
  problem <- decodeProblem <$> BSL.readFile probFile
  let hole = (\(Pair x y) -> (x, y)) <$> prHole problem
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
      }
    worldPicture
    onEvent
    initViewPort

onEvent :: Event -> World -> IO World
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
  ]

holePicture :: [(Integer, Integer)] -> Picture
holePicture xs = Pictures $
  (Line $ fromIntegerPointList $ xs <> take 1 xs) :
  [Translate x y $ ThickCircle 0.25 0.5 | (x, y) <- fromIntegerPointList xs]

gridPicture :: ((Integer, Integer), (Integer, Integer)) -> Picture
gridPicture ((minX, minY), (maxX, maxY)) = Pictures $
  [ Line $ fromIntegerPointList [(x, minY), (x, maxY)] | x <- [minX .. maxX]] <>
  [ Line $ fromIntegerPointList [(minX, y), (maxX, y)] | y <- [minY .. maxY]]

fromIntegerPoint :: Num a => (Integer, Integer) -> (a, a)
fromIntegerPoint = fromInteger *** fromInteger

fromIntegerPointList :: Num a => [(Integer, Integer)] -> [(a, a)]
fromIntegerPointList = map fromIntegerPoint

boundingGrid :: [(Integer, Integer)] -> ((Integer, Integer), (Integer, Integer))
boundingGrid xs = (minimum *** minimum) &&& (maximum *** maximum) $ unzip xs

boundingViewPort :: [(Integer, Integer)] -> ViewPort
boundingViewPort xs = case fromIntegerPoint *** fromIntegerPoint $ boundingGrid xs of
  ((minX, minY), (maxX, maxY)) -> ViewPort
    { viewPortTranslate = (- (minX + maxX) / 2, - (minY + maxY) / 2)
    , viewPortScale = min (1920 / (1 + maxX - minX)) (1080 / (1 + maxY - minY))
    , viewPortRotate = 0
    }
