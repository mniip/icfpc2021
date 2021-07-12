{-# LANGUAGE BangPatterns #-}
module ICFPC.Propagators where

import Data.Ord
import Data.Maybe
import Data.IORef
import Data.Foldable
import Data.List (sortBy)
import qualified Data.IntMap.Strict as IM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Set as S
import qualified Data.Array.IO as A
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.MVar
import System.Random

import ICFPC.Geometry (stretchAnnulus)
import ICFPC.Polygon
import ICFPC.Vector
import ICFPC.Problem
import qualified ICFPC.RLE as R
import qualified ICFPC.RLE2D as R2
import qualified ICFPC.IntPairMap as IPM
import System.IO

class Monoid a => Nodeable a where
  bottom :: a
  isBottom :: a -> Bool
  -- (<>) is commutative and idempotent
  -- bottom <> x = bottom

type NodeIdx = Int

type Updater a = IM.IntMap a -> (NodeIdx -> a -> IO ()) -> a -> a -> a -> IO ()

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
                  | otherwise = modifyIORef' pendingRef $ IM.insertWith (<>) j y
            states <- liftIO $ readIORef statesRef
            liftIO $ (updaters IM.! i) states trigger x x' x''
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

experimentPar_ :: Foldable t => CircuitState a -> t b -> (CircuitState a -> b -> IO c) -> IO ()
experimentPar_ circ xs f = forM_ xs (\x -> liftIO (cloneCircuit circ) >>= (`f` x))

data ZSet = Full | Finite R2.RLE2D
  deriving (Eq, Ord, Show)

instance Semigroup ZSet where
  Full <> s = s
  s <> Full = s
  Finite s1 <> Finite s2 = Finite $ R2.intersection s1 s2

instance Monoid ZSet where
  mempty = Full

instance Nodeable ZSet where
  bottom = Finite R2.empty
  isBottom = (bottom ==)

isZSubset :: ZSet -> ZSet -> Bool
isZSubset _ Full = True
isZSubset Full _ = False
isZSubset (Finite s1) (Finite s2) = s1 `R2.isSubsetOf` s2

zSize Full = -1
zSize (Finite s) = R2.size s

iterateCircuit :: CircuitState ZSet -> (IM.IntMap V2 -> IO ()) -> IO ()
iterateCircuit circ cb = go circ
  where
    go circ = do
      res <- runCircuit circ
      --putStrLn $ "Circuit stopped, bottom=" <> show res
      unless res $ do
        !nodes <- viewNodes circ
        --putStrLn $ "Node sizes: " <> show [R2.size s | (_, Finite s) <- nodes]
        case mapMaybe isUnresolved $ IM.toList nodes of
          [] -> cb $ IM.map getSingleton nodes
          unresolved -> do
            let (!i, !locs) = minimumBy (comparing $ R2.size . snd) unresolved
            --putStr $ "{" <> show (R2.size locs)
            --hFlush stdout
            --putStrLn $ "{ Fixing " <> show i
            locs' <- shuffle $ R2.toList locs
            experiment_ circ locs' $ \circ loc -> do
              triggerNode circ i (Finite $ R2.singleton loc)
              --putStrLn $ "Assuming " <> show i <> " -> " <> show loc
              go circ
            --putStrLn $ "} Fixing " <> show i
            --putStr "}"
    isUnresolved (i, Finite s) | R2.size s > 1 = Just (i, s)
    isUnresolved _ = Nothing
    getSingleton (Finite s) = R2.findAny s

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- A.readArray ar i
            vj <- A.readArray ar j
            A.writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (A.IOArray Int a)
    newArray n xs = A.newListArray (1,n) xs
