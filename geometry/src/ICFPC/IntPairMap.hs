{-# LANGUAGE BangPatterns, DerivingStrategies #-}
module ICFPC.IntPairMap where

import qualified Data.IntMap as IM

-- contains both (a, b) and (b, a)
newtype IntPairMap a = IntPairMap (IM.IntMap (IM.IntMap a))
  deriving stock (Eq, Ord, Show)

toList :: IntPairMap a -> [(Int, Int, a)]
toList (IntPairMap m) = [(a, b, x) | (a, s) <- IM.toList m, (b, x) <- takeWhile ((<= a) . fst) $ IM.toList s]

fromList :: [(Int, Int, a)] -> IntPairMap a
fromList = IntPairMap . go IM.empty
  where
    go !m [] = m
    go !m ((a, b, x):xs) = go (IM.alter (adding b x) a $ IM.alter (adding a x) b m) xs
    adding b x = Just . maybe (IM.singleton b x) (IM.insert b x)
