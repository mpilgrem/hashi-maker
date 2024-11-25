{-|
Module      : Hashi.State
Description : Convert a Hashiwokakero problem to an initial state
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi.State
  ( stateFromProblem
  ) where

import           Data.List ( find )
import qualified Data.Map as Map
import           Data.Maybe ( isJust )
import           Hashi.Types
                   ( BridgeSet (..), Field (..), Index, Island, IslandState (..)
                   , Problem (..), State
                   )

-- | Yields an initial state from a problem. Assumes that the top left location
-- in the grid is @(0, 0)@.
stateFromProblem :: Problem -> State
stateFromProblem p = state
 where
  grid = pGrid p
  widthGrid = pWidthGrid p
  heightGrid = pHeightGrid p
  state = Map.mapMaybeWithKey toIslandState grid
  islands = Map.assocs state

  toIslandState :: Index -> Field -> Maybe IslandState
  toIslandState i (Island n) = Just islandState
   where
    islandState =
      IslandState n (top i) (right i) (bottom i) (left i) rx bx bridgeSets
    bridgeSets = filter allBridgesToSomewhere $ nBridgeSets n
    allBridgesToSomewhere (BridgeSet t r b l) =
         (isJust (topNeighbor islandState) || t == 0)
      && (isJust (rightNeighbor islandState) || r == 0)
      && (isJust (bottomNeighbor islandState) || b == 0)
      && (isJust (leftNeighbor islandState) || l == 0)
    rx = map fst $ filter (xing (i, islandState)) islands
    bx = map fst $ filter (`xing` (i, islandState)) islands
  toIslandState _ Water = Nothing

  top (c, r) = find isIslandIndex [(c, rr) | rr <- [r - 1, r - 2 .. 0]]
  right (c, r) = find isIslandIndex [(cc, r) | cc <- [c + 1 .. widthGrid - 1]]
  bottom (c, r) = find isIslandIndex [(c, rr) | rr <- [r + 1 .. heightGrid - 1]]
  left (c, r) = find isIslandIndex [(cc, r) | cc <- [c - 1, c - 2 .. 0]]
  isIslandIndex i = isJust (Map.lookup i grid)

-- | Would a bridge to the righthand neighbour of the first island (if any)
-- cross a bridge to the bottom neighbour of the second island (if any)?
xing :: Island -> Island -> Bool
xing ((c1, r1), s1) ((c2, r2), s2) =
  case (rightNeighbor s1, bottomNeighbor s2) of
    (Just (c1', _), Just(_, r2')) -> r2 < r1 && r1 < r2' && c1 < c2 && c2 < c1'
    _                        -> False

-- | A list of all possible bridge sets consistent with the given constraint.
nBridgeSets :: Int -> [BridgeSet]
nBridgeSets n = filter p allBridgeSets
 where
  p (BridgeSet t r b l) = t + r + b + l == n

  -- A list of all possible bridge sets.
  allBridgeSets :: [BridgeSet]
  allBridgeSets =
    [ BridgeSet t r b l
    | t <- [0 .. 2]
    , r <- [0 .. 2]
    , b <- [0 .. 2]
    , l <- [0 .. 2]
    ]
