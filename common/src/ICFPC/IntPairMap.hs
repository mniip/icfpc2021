{-# LANGUAGE BangPatterns, DerivingStrategies #-}
module ICFPC.IntPairMap where

import qualified Data.IntMap as IM

-- contains both (a, b) and (b, a)
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
