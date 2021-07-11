{-# LANGUAGE BangPatterns #-}
module ICFPC.DbList where

-- make a cyclic doubly linked list
mkCycDbList :: (b -> a -> b -> b) -> [a] -> [b]
mkCycDbList f [] = []
mkCycDbList f [v] = let node = f node v node in [node]
mkCycDbList f (v:u:us) = let
  !(last, second, list) = go first first u us
  first = f last v second
  in first:list
  where
    go first prev v [] = let node = f prev v first in (node, node, [node])
    go first prev v (u:us) = let
      !(last, next, list) = go first node u us
      node = f prev v next
      in (last, node, node:list)

cycPairs :: [a] -> [(a, a)]
cycPairs = mkCycDbList (\_ x ~(y, _) -> (x, y))

cycTriples :: [a] -> [(a, a, a)]
cycTriples = mkCycDbList (\_ x ~(y, z, _) -> (x, y, z))
