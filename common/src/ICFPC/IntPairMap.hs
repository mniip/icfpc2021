{-# LANGUAGE BangPatterns, DerivingStrategies #-}
-- A map where the key is an unordered pair of ints
module ICFPC.IntPairMap where

import qualified Data.IntMap as IM

-- An intmap indexed by the first int in the pair, which contains an intmap indexed by the second int.
-- The structure contains two copies of the value to account for the two orientations.
newtype IntPairMap a = IntPairMap (IM.IntMap (IM.IntMap a))
  deriving stock (Eq, Ord, Show)

toList :: IntPairMap a -> [(Int, Int, a)]
toList (IntPairMap m) = [(a, b, x) | (a, s) <- IM.toList m, (b, x) <- takeWhile ((<= a) . fst) $ IM.toList s]

toSourcedList :: IntPairMap a -> [(Int, [(Int, a)])]
toSourcedList (IntPairMap m) = [(a, takeWhile ((<= a) . fst) $ IM.toList s) | (a, s) <- IM.toList m]

fromList :: [(Int, Int, a)] -> IntPairMap a
fromList = IntPairMap . go IM.empty
  where
    go !m [] = m
    go !m ((a, b, x):xs) = go (IM.alter (adding b x) a $ IM.alter (adding a x) b m) xs
    adding b x = Just . maybe (IM.singleton b x) (IM.insert b x)

neighbors :: IntPairMap a -> Int -> [(Int, a)]
neighbors (IntPairMap m) v =
    case m IM.!? v of
      Nothing -> []
      Just m' -> IM.toList m'

lookup :: Int -> Int -> IntPairMap a -> Maybe a
lookup a b (IntPairMap m) = IM.lookup a m >>= IM.lookup b

(!) :: IntPairMap a -> (Int, Int) -> a
(!) (IntPairMap m) (a, b) = case IM.lookup a m of
  Nothing -> error "First key not found"
  Just m' -> case IM.lookup b m' of
    Nothing -> error "Second key not found"
    Just x -> x

member :: (Int, Int) -> IntPairMap a -> Bool
member (a, b) (IntPairMap m) = case IM.lookup a m of
  Nothing -> False
  Just m' -> b `IM.member` m'
