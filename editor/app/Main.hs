{-# LANGUAGE BangPatterns, TypeApplications, TupleSections #-}
module Main where

import Control.Exception
import System.Environment
import System.Exit
import Data.IORef
import Data.List
import qualified Data.IntMap.Strict as IM
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Picture hiding (Point)
import Graphics.Gloss.Interface.IO.Interact hiding (Point)
import qualified Data.ByteString.Lazy as BSL
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Ratio
import Data.Function (on)

import ICFPC.DbList
import ICFPC.JSON
import ICFPC.Geometry
import qualified ICFPC.Vector as V
import ICFPC.Rational
import ICFPC.Polygon hiding (Polygon)
import qualified ICFPC.RLE as R
import qualified ICFPC.RLE2D as R2

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
  , wDragMode :: DragMode
  , wEpsilon :: Int
  , wBonuses :: [BonusDescription]
  , wSaveFile :: FilePath
  , wHideSimple :: Bool
  , wVisualizeDislikes :: Bool
  , wChains :: [(Int, Int, Dist)]
  , wShowTriangulation :: Bool
  , wTriangulation :: [(V.V2, V.V2, V.V2)]
  , wTriangulationElems :: Int
  , wView :: Bool
  , wSpring :: Bool
  , wEdgesToHoleEdges :: [[Point]] -- has size of wEdges
  , wShowCloseEdges :: Bool
  , wHideCloseEdgesNotUnderMouse :: Bool
  , w_a_Pressed :: Bool
  }

data DragMode = DragSimple | FollowDelta | NearestValid deriving (Eq)

nextDrag :: DragMode -> DragMode
nextDrag DragSimple = FollowDelta
nextDrag FollowDelta = DragSimple -- NearestValid -- TODO
nextDrag NearestValid = DragSimple


neighbours :: [(Int, Int, Dist)] -> Int -> [(Int, Dist)]
neighbours edges node = filterMap (\(i, j, d) -> if      i == node then Just (j, d)
                                                 else if j == node then Just (i, d)
                                                 else Nothing) edges
  where
    filterMap f l = map (maybe (0, 0) id) $ filter (/= Nothing) $ map f l

isEdgeToSimple :: [(Int, Int, Dist)] -> (Int, Int) -> Bool
isEdgeToSimple edges (u, v) = length (neighbours edges u) <= 2 || length (neighbours edges v) <= 2

calcChains :: [(Int, Int, Dist)] -> [Point] -> [(Int, Int, Dist)]
calcChains edges verts = edges -- TODO

main :: IO ()
main = do
  [probFile, solFile] <- getArgs
  vpRef <- newIORef $ error "no viewport"
  problem <- decodeProblem <$> BSL.readFile probFile
  eSolution <- try @SomeException $ evaluate =<< decodePose <$> BSL.readFile solFile
  let hole = prHole problem
  let initViewPort ctrl = do
        writeIORef vpRef (controllerModifyViewPort ctrl)
        controllerModifyViewPort ctrl $ \_ -> pure $ boundingViewPort hole
  let origVertices = fromIntegerPointList $ figVertices (prFigure problem)
  let vertices = case eSolution of
        Left _ -> origVertices
        Right sol -> fromIntegerPointList $ poseVertices sol
  let edges = (\(u, v) -> (u, v, dist (origVertices !! u) (origVertices !! v))) <$> figEdges (prFigure problem)
  interactIO
    FullScreen
    black
    World
      { wModifyViewPort = \vp -> do
          modVP <- readIORef vpRef
          modVP vp
      , wHole = hole
      , wGrid = boundingGrid hole
      , wEdges = edges
      , wVertices = vertices
      , wMouseCoords = (0, 0)
      , wDragging = Nothing
      , wDragMode = DragSimple
      , wEpsilon = prEpsilon problem
      , wBonuses = prBonuses problem
      , wSelection = S.empty
      , wSelectionRect = Nothing
      , wSaveFile = solFile
      , wHideSimple = False
      , wVisualizeDislikes = False
      , wChains = calcChains edges vertices
      , wShowTriangulation = False
      , wTriangulation = triangulate (mkPolygon $ V.packV2 <$> hole)
      , wTriangulationElems = -1
      , wView = False
      , wSpring = False
      , wEdgesToHoleEdges = calculateCloseEdges (prEpsilon problem) edges (cyclePairs hole)
      , wShowCloseEdges = False
      , wHideCloseEdgesNotUnderMouse = False
      , w_a_Pressed = False
      }
    worldPicture
    onEvent
    initViewPort

onEvent :: Event -> World -> IO World
onEvent (EventMotion coords) world = do
  newCoords <- getMousePoint world coords
  let newVertices = if wSpring world
                    then adjustPoints (wEpsilon world) (wEdges world) (wVertices world) (wHole world)
                    else wVertices world
      world' = world { wMouseCoords = newCoords, wVertices = newVertices }
  case wDragging world' of
    Nothing -> case wSelectionRect world' of
      Nothing -> pure world'
      Just (tl, _) -> pure world' { wSelectionRect = Just (tl, newCoords) }
    Just prev -> do
      let delta = newCoords .-. prev
      let newVertices = if delta == (0, 0) then (wVertices world') else
                        case wDragMode world of
                          DragSimple   -> moveSelected (.+. delta) (wSelection world', wSelection world') (wVertices world')
                          FollowDelta  -> applyMoverBfs (moveSelected (.+. delta)) (wSelection world', wSelection world') (wVertices world') world'
                          NearestValid -> applyMoverBfs (moveToNearestValid world') (wSelection world', wSelection world') (wVertices world') world'
        --foldl' (\xs i -> withNth i (.+. delta) xs) (wVertices world') $ S.toList $ wSelection world'
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
        , wSelection = if w_a_Pressed world then selection `S.union` (wSelection world) else selection
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
onEvent (EventKey (MouseButton RightButton) Down _ coords) world = do
  cursor <- getMousePoint world coords
  pure world { wDragging = Just cursor }
onEvent (EventKey (MouseButton RightButton) Up _ _) world = pure world { wDragging = Nothing }
onEvent (EventKey (SpecialKey KeyEsc) Down _ _) world = exitSuccess
onEvent (EventKey (Char 's') Down _ _) world = do
  BSL.writeFile (wSaveFile world) $ encodePose Pose
    { poseVertices = wVertices world
    , poseBonuses = []
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
onEvent (EventKey (Char '1') Down _ coord) world = do
  let newVertices = adjustPoints (wEpsilon world) (wEdges world) (wVertices world) (wHole world)
  pure world { wVertices = newVertices }
onEvent (EventKey (Char '2') Down _ coord) world = do
  let newVertices = improvePoints (wEpsilon world) (wEdges world) (wVertices world) (wHole world)
  pure world { wVertices = newVertices }
onEvent (EventKey (Char '3') Down _ coord) world = do
  let newVertices = putPointsAway (wEpsilon world) (wEdges world) (wVertices world) (wHole world)
  pure world { wVertices = newVertices }
onEvent (EventKey (Char 'h') Up _ _) world = pure world { wHideSimple         = not (wHideSimple world) }
onEvent (EventKey (Char 'd') Up _ _) world = pure world { wVisualizeDislikes  = not (wVisualizeDislikes world) }
onEvent (EventKey (Char 'g') Up _ _) world = pure world { wDragMode           = nextDrag (wDragMode world) }
onEvent (EventKey (Char 't') Up _ _) world = pure world { wShowTriangulation  = not (wShowTriangulation world) }
onEvent (EventKey (Char '>') Up _ _) world = pure world { wTriangulationElems = min ((wTriangulationElems world) + 1) (length $ wTriangulation world)}
onEvent (EventKey (Char '<') Up _ _) world = pure world { wTriangulationElems = max ((wTriangulationElems world) - 1) (-1)}
onEvent (EventKey (Char 'v') Up _ _) world = pure world { wView           = not (wView world) }
onEvent (EventKey (Char 'r') Up _ _) world = pure world { wSpring         = not (wSpring world) }
onEvent (EventKey (Char 'c') Up _ _) world = pure world { wShowCloseEdges = not (wShowCloseEdges world) }
onEvent (EventKey (Char 'x') Up _ _) world = pure world { wHideCloseEdgesNotUnderMouse = not (wHideCloseEdgesNotUnderMouse world) }
onEvent (EventKey (Char 'a') Up _ _) world = pure world { w_a_Pressed = False }
onEvent (EventKey (Char 'a') Down _ _) world = pure world { w_a_Pressed = True }
onEvent event world = pure world

getMousePoint :: World -> (Float, Float) -> IO Point
getMousePoint world coords= do
  vpRef <- newIORef undefined
  wModifyViewPort world $ \vp -> writeIORef vpRef vp >> pure vp
  vp <- readIORef vpRef
  pure $ round *** round $ invertViewPort vp coords

worldPicture :: World -> IO Picture
worldPicture world = pure $ Pictures $
  [ Color (greyN 0.25) $ gridPicture (wGrid world)
  , Color red $ holePicture (wHole world)
  , boundariesPicture (wEpsilon world) (showBoundary (wSelection world) (wDragging world)) (filterHide $ wEdges world) (wVertices world) (wHole world)
  , figurePicture (wEpsilon world) (wEdges world) (wVertices world) (wSelection world) (wHideSimple world)
  , Color white $ selectionPicture (wSelectionRect world)
  , cursorPicture $ wMouseCoords world
  , viewPicture (wView world) (wHole world) (wMouseCoords world)
  , Color white $ scorePicture (wGrid world) (valid (wEpsilon world) (wHole world) (wEdges world) (wVertices world)) (dislikes (wHole world) (wVertices world))
  ] ++ circles ++ holeTriangulation ++ map showBonus (wBonuses world) ++ similarEdges
  where
    showBoundary s (Just _) | S.size s == 1 = Just (S.findMin s)
    showBoundary _ _ = Nothing
    filterHide :: [(Int, Int, Dist)] -> [(Int, Int, Dist)]
    filterHide = if wHideSimple world then filter (\(u, v, d) -> not $ isEdgeToSimple (wEdges world) (u, v))
                 else id
    circles = if wVisualizeDislikes world then [visualizeDislikesPicture (wHole world) (wVertices world)] else []
    holeTriangulation = if wShowTriangulation world then [triangulationPicture (triangulationTail $ wTriangulation world)] else []
    triangulationTail tr = if wTriangulationElems world == -1 then tr else drop (length tr - wTriangulationElems world) tr
    showBonus bonus = let (x, y) = fromIntegerPoint $ bdPosition bonus
                          color = case bdBonus bonus of
                                      Globalist -> yellow
                                      BreakALeg -> blue
                                      WallHack -> orange
                                      SuperFlex -> cyan
                      in Color (withAlpha 0.5 color) $ Translate x y $ ThickCircle 1 2
    rHoles = 10
    edgesToHoles = if wHideCloseEdgesNotUnderMouse world then map (filter (\p -> dist p (wMouseCoords world) < rHoles * rHoles)) (wEdgesToHoleEdges world) else wEdgesToHoleEdges world
    similarEdges = if wShowCloseEdges world then [closeEdgesPictureHighlight (edgesLines (wEdges world) (wVertices world)) edgesToHoles, holeCircles] else []
    holeCircles = Pictures $ map (\(p, q) -> let (x, y) = fromIntegerPoint (middlePoint p q) in Translate x y $ Circle (fromIntegral rHoles)) (cyclePairs (wHole world))

validShort :: World -> [Point] -> (Bool, Bool)
validShort world verts = valid (wEpsilon world) (wHole world) (wEdges world) verts

scorePicture :: (Point, Point) -> (Bool, Bool) -> Int -> Picture
scorePicture ((minX, _), (_, maxY)) (inside, stretch) score =
    Translate (fromIntegral minX) (fromIntegral $ maxY + 1) $ Scale 0.02 0.02 $ Text $ show (isinside, isstretch, score)
        where isinside = if inside then "OK" else "Not inside"
              isstretch = if stretch then "OK" else "Lengths"

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

visualizeDislikesPicture :: [Point] -> [Point] -> Picture
visualizeDislikesPicture hole pose = Pictures $ map (\((x, y), r) -> Color blue $ Translate x y $ ThickCircle (sqrt $ fromIntegral r) 0.5) circles
  where
    circles = zip (fromIntegerPointList hole) [ minimum [dist h v | v <- pose] | h <- hole]

triangulationColor = white
triangulationPicture :: [(V.V2, V.V2, V.V2)] -> Picture
triangulationPicture triangles = Pictures $ map (Color triangulationColor) $ concatMap (\(p1, p2, p3) -> [Line [p1, p2], Line [p2, p3], Line [p1, p3]]) (map fromIntegerTriangles triangles)
  where
    fromIntegerTriangles (V.V2 p1x p1y, V.V2 p2x p2y, V.V2 p3x p3y) =
      ((fromIntegral p1x, fromIntegral p1y), (fromIntegral p2x, fromIntegral p2y), (fromIntegral p3x, fromIntegral p3y))

figurePicture :: Epsilon -> [(Int, Int, Dist)] -> [Point] -> S.Set Int -> Bool -> Picture
figurePicture eps is xs selected hideSimple = Pictures $
  [ Color (edgeColor u v origD) $ Line $
    fromIntegerPointList [xs !! u, xs !! v]
  | (u, v, origD) <- is ] <>
  [Color (selectColor i) $ Translate x y $ ThickCircle 0.25 0.5 | (i, (x, y)) <- zip [0..] $ fromIntegerPointList xs]
  where
    edgeColor u v origD = if hideSimple && isEdgeToSimple is (u, v) then black else stretchColor $ canStretch eps origD (xs !! u, xs !! v)
    stretchColor LT = yellow
    stretchColor GT = cyan
    stretchColor EQ = green
    selectColor i = if i `S.member` selected then white else green

boundariesPicture :: Epsilon -> Maybe Int -> [(Int, Int, Dist)] -> [Point] -> Polygon -> Picture
boundariesPicture _ Nothing _ _ _ = Blank
boundariesPicture eps (Just i) is xs bs = Pictures
  [ Color clr $ Polygon [(x - 0.5, y - 0.5), (x + 0.5, y - 0.5), (x + 0.5, y + 0.5), (x - 0.5, y + 0.5), (x - 0.5, y - 0.5)]
  | (coord, p) <- M.toList coords
  , let q = fromIntegral (p - 1) / fromIntegral (length jss - 1)
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
  (x, y) -> Pictures
    [ Color blue $ Translate x y $ ThickCircle 0.25 0.5
    , Color white $ Translate x y $ Scale 0.02 0.02 $ Text $ show coords
    ]

viewPicture :: Bool -> Polygon -> Point -> Picture
viewPicture False _ _ = Blank
viewPicture True bs coords = Color (withAlpha 0.5 yellow) $ Pictures $
    [ Polygon [(x1 - 0.5, y - 0.5), (x2 - 0.5, y - 0.5), (x2 - 0.5, y + 0.5), (x1 - 0.5, y + 0.5), (x1 - 0.5, y - 0.5)]
    | (ix1, ix2, iy) <- R2.toRuns $ computePolygonVisibility (mkPolyCCW $ mkPolygon $ V.packV2 <$> bs) (V.packV2 coords)
    , let (x1, x2, y) = (fromIntegral ix1, fromIntegral ix2, fromIntegral iy)
    ]

closeDistEdgesColor = magenta
closeEdgesPictureHighlight :: [(Point, Point)] -> [[Point]] -> Picture
closeEdgesPictureHighlight edges hole_edges = Pictures $ map (\(p1, p2) -> Color closeDistEdgesColor $ Line $ fromIntegerPointList [p1, p2]) lines
  where
    lines = map (\(e, hs) -> e) $ filter (\(_, hs) -> not (null hs)) $ zip edges hole_edges

fromIntegerPoint :: Num a => Point -> (a, a)
fromIntegerPoint = fromIntegral *** fromIntegral

fromIntegerPointList :: Num a => [Point] -> [(a, a)]
fromIntegerPointList = map fromIntegerPoint

withNth :: Int -> (a -> a) -> [a] -> [a]
withNth n f = go n
  where
    go !_ [] = []
    go !0 (x:xs) = f x:xs
    go !n (x:xs) = x:go (n-1) xs

boundingGrid :: [(Int, Int)] -> ((Int, Int), (Int, Int))
boundingGrid xs = (minimum *** minimum) &&& (maximum *** maximum) $ unzip xs

boundingViewPort :: [(Int, Int)] -> ViewPort
boundingViewPort xs = case fromIntegerPoint *** fromIntegerPoint $ boundingGrid xs of
  ((minX, minY), (maxX, maxY)) -> ViewPort
    { viewPortTranslate = (- (minX + maxX) / 2, - (minY + maxY) / 2)
    , viewPortScale = min (1920 / (1 + maxX - minX)) (1080 / (1 + maxY - minY))
    , viewPortRotate = 0
    }

getNeighbours :: [(Int, Int, Dist)] -> S.Set Int -> (S.Set Int, S.Set Int)
getNeighbours edges nodes = foldl (addIfNeighbour) (nodes, S.empty) edges
  where
    addIfNeighbour :: (S.Set Int, S.Set Int) -> (Int, Int, Dist) -> (S.Set Int, S.Set Int)
    addIfNeighbour acc@(all, new) (l, r, _) = if      l `S.member` nodes && not (r `S.member` nodes) then (r `S.insert` all, r `S.insert` new)
                                              else if r `S.member` nodes && not (l `S.member` nodes) then (l `S.insert` all, l `S.insert` new)
                                              else acc

getNeighboursWithDists :: [(Int, Int, Dist)] -> Int -> [(Int, Dist)]
getNeighboursWithDists edges node = filterMap (\(i, j, d) -> if i == node then Just (j, d)
                                                             else if j == node then Just (i, d)
                                                             else Nothing) edges
  where
    filterMap f l = map (maybe (0, 0) id) $ filter (/= Nothing) $ map f l

moveSelected :: (Point -> Point) -> (S.Set Int, S.Set Int) -> [Point] -> [Point]
moveSelected mover (_, selected) verts = foldl' (\xs i -> if S.member i selected then withNth i mover xs else xs) verts $ S.toList $ selected

-- TODO
moveToNearestValid :: World -> (S.Set Int, S.Set Int) -> [Point] -> [Point]
moveToNearestValid w (all, new) verts = moveSelected moveToValidByAll (all, new) verts
  where
    moveToValidByAll p = getNearest (allowedPositions (wHole w) (wEpsilon w) neighbs)
      where
        Just p_idx = elemIndex p verts
        neighbs = map (\(i, d) -> (verts !! i, d)) $ filter (\(i, d) -> i `S.member` (all `S.difference` new)) $ getNeighboursWithDists (wEdges w) p_idx
        getNearest ps = if null ps then p .+. (0, 1) else minimumBy (\p1 p2 -> compare (dist p p1) (dist p p2)) ps

applyMoverBfs :: ((S.Set Int, S.Set Int) -> [Point] -> [Point]) -> (S.Set Int, S.Set Int) -> [Point] -> World -> [Point]
applyMoverBfs mover (all, new) result_acc world = if S.size all == length result_acc then wVertices world
                                                  else if dist_ok then movedPoints
                                                  else applyMoverBfs mover (getNeighbours (wEdges world) all) movedPoints world
  where
    movedPoints = mover (all, new) result_acc
    (inside_ok, dist_ok) = validShort world movedPoints

edgesMiddlePoints :: [(Int, Int, Dist)] -> [Point] -> [Point]
edgesMiddlePoints edges verts = map (\(i, j, _) -> middlePoint (verts !! i) (verts !! j)) edges

edgesLines :: [(Int, Int, Dist)] -> [Point] -> [(Point, Point)]
edgesLines edges verts = map (\(i, j, _) -> (verts !! i, verts !! j)) edges

middlePoint :: Point -> Point -> Point
middlePoint (p1x, p1y) (p2x, p2y) = ((p1x + p2x) `div` 2, (p1y + p2y) `div` 2)

calculateCloseEdges :: Epsilon -> [(Int, Int, Dist)] -> [(Point, Point)] -> [[Point]]
calculateCloseEdges e edges hole_edges = map (\(_, _, d) -> getMiddles d) edges
  where
    getMiddles d = map (\(p, q) -> middlePoint p q) $ filter (\s -> canStretch e d s == EQ) hole_edges
