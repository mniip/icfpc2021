{-# LANGUAGE BangPatterns #-}
module ICFPC.Propagators where

import Data.Ord
import Data.Maybe
import Data.IORef
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Set as S
import qualified Data.Array as A
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.MVar

import ICFPC.Geometry (stretchAnnulus)
import ICFPC.Polygon
import ICFPC.Vector
import ICFPC.Problem
import qualified ICFPC.RLE as R
import qualified ICFPC.RLE2D as R2
import qualified ICFPC.IntPairMap as IPM

class Monoid a => Nodeable a where
  bottom :: a
  isBottom :: a -> Bool
  -- (<>) is commutative and idempotent
  -- bottom <> x = bottom

type NodeIdx = Int

type Updater a = (NodeIdx -> a -> IO ()) -> a -> a -> a -> IO ()

data CircuitState a = CircuitState
  { cNodeIdx :: IORef NodeIdx
  , cNodeUpdaters :: IORef (IM.IntMap (Updater a))
  , cNodeStates :: IORef (IM.IntMap a)
  , cPendingUpdates :: IORef (IM.IntMap a)
  }

newCircuitState :: IO (CircuitState a)
newCircuitState = do
  idx <- newIORef 0
  updaters <- newIORef IM.empty
  states <- newIORef IM.empty
  updates <- newIORef IM.empty
  pure CircuitState
    { cNodeIdx = idx
    , cNodeUpdaters = updaters
    , cNodeStates = states
    , cPendingUpdates = updates
    }

addNode :: CircuitState a -> a -> Updater a -> IO NodeIdx
addNode circ x upd = do
  idx <- atomicModifyIORef' (cNodeIdx circ) $ \i -> (i + 1, i)
  modifyIORef' (cNodeUpdaters circ) $ IM.insert idx upd
  modifyIORef' (cNodeStates circ) $ IM.insert idx x
  pure idx

viewNodes :: CircuitState a -> IO (IM.IntMap a)
viewNodes circ = readIORef $ cNodeStates circ

triggerNode :: Nodeable a => CircuitState a -> NodeIdx -> a -> IO ()
triggerNode circ i x = do
  modifyIORef' (cPendingUpdates circ) $ IM.insertWith (<>) i x

-- propagate updates until they settle (False) or a node is triggered with bottom (True)
runCircuit :: Nodeable a => CircuitState a -> IO Bool
runCircuit circ = do
  !updaters <- readIORef $ cNodeUpdaters circ
  let
    !statesRef = cNodeStates circ
    !pendingRef = cPendingUpdates circ
    go = do
      keys <- IM.keys <$> readIORef pendingRef
      if null keys
      then pure False
      else do
        result <- runMaybeT $ forM_ keys $ \i -> do
          mValue <- liftIO $ IM.lookup i <$> readIORef pendingRef
          forM_ mValue $ \x' -> do
            x <- liftIO $ (IM.! i) <$> readIORef statesRef
            let !x'' = x <> x'
            when (isBottom x'') $ fail ""
            liftIO $ modifyIORef' pendingRef $ IM.delete i
            liftIO $ modifyIORef' statesRef $ IM.insertWith (<>) i x''
            let trigger j y
                  | i == j = error "cyclic trigger"
                  | otherwise = modifyIORef pendingRef $ IM.insertWith (<>) j y
            liftIO $ (updaters IM.! i) trigger  x x' x''
        case result of
          Nothing -> pure True
          Just _ -> go
  go

cloneCircuit :: CircuitState a -> IO (CircuitState a)
cloneCircuit circ = do
  idx <- newIORef =<< readIORef (cNodeIdx circ)
  updaters <- newIORef =<< readIORef (cNodeUpdaters circ)
  states <- newIORef =<< readIORef (cNodeStates circ)
  updates <- newIORef =<< readIORef (cPendingUpdates circ)
  pure CircuitState
    { cNodeIdx = idx
    , cNodeUpdaters = updaters
    , cNodeStates = states
    , cPendingUpdates = updates
    }

experiment :: Traversable t => CircuitState a -> t b -> (CircuitState a -> b -> IO c) -> IO (t c)
experiment circ xs f = forM xs (\x -> liftIO (cloneCircuit circ) >>= (`f` x))

experiment_ :: Foldable t => CircuitState a -> t b -> (CircuitState a -> b -> IO c) -> IO ()
experiment_ circ xs f = forM_ xs (\x -> liftIO (cloneCircuit circ) >>= (`f` x))

data ZSet a = Full | Finite (S.Set a)
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (ZSet a) where
  Full <> s = s
  s <> Full = s
  Finite s1 <> Finite s2 = Finite $ S.intersection s1 s2

instance Ord a => Monoid (ZSet a) where
  mempty = Full

instance Ord a => Nodeable (ZSet a) where
  bottom = Finite S.empty
  isBottom = (bottom ==)

isZSubset :: Ord a => ZSet a -> ZSet a -> Bool
isZSubset _ Full = True
isZSubset Full _ = False
isZSubset (Finite s1) (Finite s2) = s1 `S.isSubsetOf` s2

admissibleRing :: Epsilon -> Dist -> R2.RLE2D
admissibleRing eps d = R2.fromList $ map packV2 $ stretchAnnulus eps d

vertexCircuit :: ProblemSpec -> IO (CircuitState (ZSet V2))
vertexCircuit !spec = do
  circ <- newCircuitState
  let !insides = computePolygonInternals $ psHole spec
  let S2 minX minY maxX maxY = psBoundingBox spec
  let bounds = ((minX, minY), (maxX, maxY))
  let !validSegments = A.listArray bounds
        [ computePolygonVisibility (psHole spec) (V2 x y)
        | x <- [minX..maxX], y <- [minY..maxY]
        ]
  let isValidSegment (V2 x1 y1) v2@(V2 x2 y2)
        = A.inRange bounds (x1, y1) && A.inRange bounds (x2, y2) && v2 `R2.member` (validSegments A.! (x1, y1))
  let numVs = length $ psOriginalVertices spec
  forM_ [0 .. numVs - 1] $ \i -> do
    let !neighbors =
          [ (j, admDelta)
          | (j, dist) <- IPM.neighbors (psEdges spec) i
          , let !admDelta = admissibleRing (psEpsilon spec) dist
          ]
    addNode circ Full $ \trigger old intersected new -> if old `isZSubset` intersected
      then pure ()
      else case new of
        Full -> pure () -- unreachable
        Finite new' -> do
          forM_ neighbors $ \(j, admDelta) -> do
            let !admissible = S.fromList
                  [ other
                  | pos <- S.toList new'
                  , delta <- R2.toList admDelta
                  , let !other = pos .+. delta
                  , isValidSegment pos other
                  ]
            trigger j $ Finite admissible
  forM_ [0 .. numVs - 1] $ \i -> triggerNode circ i $ Finite $ S.fromList . R2.toList $ insides
  pure circ
  where
    neighborOf i (j, k, d)
      | i == j = Just (k, d)
      | i == k = Just (j, d)
      | otherwise = Nothing

iterateCircuit :: Ord a => CircuitState (ZSet a) -> ([a] -> IO ()) -> IO ()
iterateCircuit circ cb = go circ
  where
    go circ = do
      res <- runCircuit circ
      --putStrLn $ "Circuit stopped, bottom=" <> show res
      unless res $ do
        !nodes <- IM.toList <$> viewNodes circ
        --putStrLn $ "Node sizes: " <> show [S.size s | (_, Finite s) <- nodes]
        case mapMaybe isUnresolved nodes of
          [] -> cb $ getSingleton . snd <$> nodes
          unresolved -> do
            let (!i, !locs) = minimumBy (comparing $ S.size . snd) unresolved
            --putStrLn $ "{ Forking in " <> show (S.size locs) <> " ways"
            --putStrLn $ "{ Fixing " <> show i
            experiment_ circ locs $ \circ loc -> do
              triggerNode circ i (Finite $ S.singleton loc)
              --putStrLn $ "Assuming " <> show i <> " -> " <> show loc
              go circ
            --putStrLn $ "} Fixing " <> show i
            --putStrLn "}"
    isUnresolved (i, Finite s) | S.size s > 1 = Just (i, s)
    isUnresolved _ = Nothing
    getSingleton (Finite s) | Just m <- S.lookupMin s = m
    getSingleton _ = error "getSingleton Full/empty"
