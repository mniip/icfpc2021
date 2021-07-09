{-# LANGUAGE BangPatterns, TypeApplications, TupleSections #-}
module Main where

import Control.Exception
import System.Environment
import System.Exit
import Data.IORef
import Data.List
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Picture hiding (Point)
import Graphics.Gloss.Interface.IO.Interact hiding (Point)
import qualified Data.ByteString.Lazy as BSL
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import ICFPC.JSON
import ICFPC.Geometry

data World = World
  { wModifyViewPort :: (ViewPort -> IO ViewPort) -> IO ()
  , wHole :: Polygon
  , wGrid :: (Point, Point)
  , wEdges :: [(Int, Int, Dist)]
  , wVertices :: [Point]
  , wMouseCoords :: Point
  , wSelection :: S.Set Int
  , wSelectionRect :: Maybe (Point, Point)
  , wDragging :: Maybe Point
  , wEpsilon :: Integer
  , wSaveFile :: FilePath
  }

main :: IO ()
main = do
  [probFile, solFile] <- getArgs
  vpRef <- newIORef $ error "no viewport"
  problem <- decodeProblem <$> BSL.readFile probFile
  eSolution <- try @SomeException $ evaluate =<< decodeSolution <$> BSL.readFile solFile
  let hole = fromPair <$> prHole problem
  let initViewPort ctrl = do
        writeIORef vpRef (controllerModifyViewPort ctrl)
        controllerModifyViewPort ctrl $ \_ -> pure $ boundingViewPort hole
  let vertices = fromIntegerPointList $ fromPair <$> case eSolution of
        Left _ -> figVertices (prFigure problem)
        Right sol -> solVertices sol
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
      , wSelection = S.empty
      , wSelectionRect = Nothing
      , wSaveFile = solFile
      }
    worldPicture
    onEvent
    initViewPort

onEvent :: Event -> World -> IO World
onEvent (EventMotion coords) world = do
  newCoords <- getMousePoint world coords
  let world' = world { wMouseCoords = newCoords }
  case wDragging world' of
    Nothing -> case wSelectionRect world' of
      Nothing -> pure world'
      Just (tl, _) -> pure world' { wSelectionRect = Just (tl, newCoords) }
    Just prev -> do
      let delta = newCoords .-. prev
      let newVertices = foldl' (\xs i -> withNth i (.+. delta) xs) (wVertices world') $ S.toList $ wSelection world'
      pure world'
        { wVertices = newVertices
        , wDragging = Just newCoords
        }
onEvent (EventKey (MouseButton LeftButton) Down _ coords) world = do
  newCoords <- getMousePoint world coords
  case elemIndex (wMouseCoords world) (wVertices world) of
    Nothing -> pure world { wSelectionRect = Just (newCoords, newCoords) }
    Just i -> if S.null (wSelection world) || i `S.notMember` wSelection world
      then pure world
            { wSelection = S.singleton i
            , wDragging = Just newCoords
            }
      else pure world { wDragging = Just newCoords }
onEvent (EventKey (MouseButton LeftButton) Up _ _) world = do
  case wSelectionRect world of
    Nothing -> pure world { wDragging = Nothing }
    Just ((x1, y1), (x2, y2)) -> do
      let (minX, minY) = (min x1 x2, min y1 y2)
      let (maxX, maxY) = (max x1 x2, max y1 y2)
      let inRect (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY
      let selection = S.fromList $ map fst $ filter (inRect . snd) $ zip [0..] $ wVertices world
      pure world
        { wDragging = Nothing
        , wSelectionRect = Nothing
        , wSelection = selection
        }
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
onEvent (EventKey (SpecialKey KeyEsc) Down _ _) world = exitSuccess
onEvent (EventKey (Char 's') Down _ _) world = do
  BSL.writeFile (wSaveFile world) $ encodeSolution Solution
    { solVertices = (\(x, y) -> Pair x y) <$> wVertices world
    }
  pure world
onEvent (EventKey (Char 'q') Down _ coord) world = do
  (mx, my) <- getMousePoint world coord
  let rotate (x, y) = (mx - (y - my), my + (x - mx))
  let newVertices = foldl' (\xs i -> withNth i rotate xs) (wVertices world) $ S.toList $ wSelection world
  pure world { wVertices = newVertices }
onEvent (EventKey (Char 'e') Down _ coord) world = do
  (mx, my) <- getMousePoint world coord
  let rotate (x, y) = (mx + (y - my), my - (x - mx))
  let newVertices = foldl' (\xs i -> withNth i rotate xs) (wVertices world) $ S.toList $ wSelection world
  pure world { wVertices = newVertices }
onEvent (EventKey (Char 'f') Down _ coord) world = do
  (mx, _) <- getMousePoint world coord
  let flipping (x, y) = (2 * mx - x, y)
  let newVertices = foldl' (\xs i -> withNth i flipping xs) (wVertices world) $ S.toList $ wSelection world
  pure world { wVertices = newVertices }
onEvent event world = pure world

getMousePoint :: World -> (Float, Float) -> IO Point
getMousePoint world coords= do
  vpRef <- newIORef undefined
  wModifyViewPort world $ \vp -> writeIORef vpRef vp >> pure vp
  vp <- readIORef vpRef
  pure $ round *** round $ invertViewPort vp coords

worldPicture :: World -> IO Picture
worldPicture world = pure $ Pictures
  [ Color (greyN 0.25) $ gridPicture (wGrid world)
  , Color red $ holePicture (wHole world)
  , boundariesPicture (wEpsilon world) (showBoundary (wSelection world) (wDragging world)) (wEdges world) (wVertices world) (wHole world)
  , figurePicture (wEpsilon world) (wEdges world) (wVertices world) (wSelection world)
  , Color white $ selectionPicture (wSelectionRect world)
  , cursorPicture $ wMouseCoords world
  , Color white $ scorePicture (wGrid world) (dislikes (wHole world) (wVertices world))
  ]
  where
    showBoundary s (Just _) | S.size s == 1 = Just (S.findMin s)
    showBoundary _ _ = Nothing

scorePicture :: (Point, Point) -> Integer -> Picture
scorePicture ((minX, _), (_, maxY)) score = Translate (fromInteger minX) (fromInteger $ maxY + 1) $ Scale 0.02 0.02 $ Text $ show score

gridPicture :: (Point, Point) -> Picture
gridPicture ((minX, minY), (maxX, maxY)) = Pictures $
  [ Line $ fromIntegerPointList [(x, minY), (x, maxY)] | x <- [minX .. maxX]] <>
  [ Line $ fromIntegerPointList [(minX, y), (maxX, y)] | y <- [minY .. maxY]]

selectionPicture :: Maybe (Point, Point) -> Picture
selectionPicture Nothing = Blank
selectionPicture (Just ((x1, y1), (x2, y2))) = Line
  [ (minX - 0.5, minY - 0.5)
  , (maxX + 0.5, minY - 0.5)
  , (maxX + 0.5, maxY + 0.5)
  , (minX - 0.5, maxY + 0.5)
  , (minX - 0.5, minY - 0.5)
  ]
  where
    (minX, minY) = fromIntegerPoint (min x1 x2, min y1 y2)
    (maxX, maxY) = fromIntegerPoint (max x1 x2, max y1 y2)

figurePicture :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> S.Set Int -> Picture
figurePicture eps is xs selected = Pictures $
  [ Color (stretchColor $ canStretch eps origD (xs !! u, xs !! v)) $ Line $
    fromIntegerPointList [xs !! u, xs !! v]
  | (u, v, origD) <- is ] <>
  [Color (selectColor i) $ Translate x y $ ThickCircle 0.25 0.5 | (i, (x, y)) <- zip [0..] $ fromIntegerPointList xs]
  where
    stretchColor LT = yellow
    stretchColor GT = cyan
    stretchColor EQ = green
    selectColor i = if i `S.member` selected then white else green

boundariesPicture :: Epsilon -> Maybe Int -> [(Int, Int, Dist)] -> [Point] -> Polygon -> Picture
boundariesPicture _ Nothing _ _ _ = Blank
boundariesPicture eps (Just i) is xs bs = Pictures
  [ Color clr $ Polygon [(x - 0.5, y - 0.5), (x + 0.5, y - 0.5), (x + 0.5, y + 0.5), (x - 0.5, y + 0.5), (x - 0.5, y - 0.5)]
  | (coord, p) <- M.toList coords
  , let q = fromIntegral p / fromIntegral (length jss)
  , let clr = withAlpha 0.5 $ mixColors (1 - q) q red green
  , let (x, y) = fromIntegerPoint coord
  ]
  where
    coords = M.unionsWith (+) [M.fromList $ (,1) <$> allowedPositions bs eps [(j, d)] | (j, d) <- jss]
    jss = [(xs !! j, d) | (i', j, d) <- is, i' == i] <> [(xs !! j, d) | (j, i', d) <- is, i' == i]

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

withNth :: Int -> (a -> a) -> [a] -> [a]
withNth n f = go n
  where
    go !_ [] = []
    go !0 (x:xs) = f x:xs
    go !n (x:xs) = x:go (n-1) xs

boundingGrid :: [(Integer, Integer)] -> ((Integer, Integer), (Integer, Integer))
boundingGrid xs = (minimum *** minimum) &&& (maximum *** maximum) $ unzip xs

boundingViewPort :: [(Integer, Integer)] -> ViewPort
boundingViewPort xs = case fromIntegerPoint *** fromIntegerPoint $ boundingGrid xs of
  ((minX, minY), (maxX, maxY)) -> ViewPort
    { viewPortTranslate = (- (minX + maxX) / 2, - (minY + maxY) / 2)
    , viewPortScale = min (1920 / (1 + maxX - minX)) (1080 / (1 + maxY - minY))
    , viewPortRotate = 0
    }
