{-|
Module      : Hashi
Description : A solver for Hashiwokakero puzzles
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi
  ( solveProblem
  ) where

import           Data.List ( find )
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Map as Map
import           Data.Maybe ( mapMaybe )
import qualified Data.Set as Set
import           Data.Set ( Set )
import           Hashi.State ( stateFromProblem )
import           Hashi.Types
                   ( BridgeSet (..), GetBridge, Index, Island, IslandState (..)
                   , Problem, State, isUncertain
                   )

solveProblem :: Problem -> [State]
solveProblem = concatMap solveState . narrowAll . stateFromProblem
 where
  narrowAll :: State -> [State]
  narrowAll state = narrow (Map.keysSet state) state

solveState :: State -> [State]
solveState state
  | Map.null state = []
  | otherwise = case (connectedComponents, find uncertain islands) of
      -- Nothing uncertain and one set of connected components: solved
      (_ :| [], Nothing) -> [state]
      -- Otherwise, if nothing uncertain: no solution
      (_, Nothing) -> []
      -- Something uncertain and one set of connected components: try to solve
      (_ :| [], Just island) -> solveForIsland island
      -- Otherwise, if something uncertain ...
      (ccs, Just island)  -> if all unfinished ccs
        -- All of the sets of connected components are unfinished: try to solve
        then solveForIsland island
        -- One or more of the sets of connected components are finished: no
        -- solution
        else []
 where
  islands = Map.assocs state
  uncertain (_, islandState) = isUncertain islandState
  unfinished = any (isUncertain . (state Map.!))

  connectedComponents :: NonEmpty (Set Index)
  connectedComponents =
    let (seed, restUnvisited) = Set.deleteFindMin (Map.keysSet state)
    in  connComps (Set.empty :| []) (Set.singleton seed) restUnvisited

  connComps ::
       NonEmpty (Set Index)
    -> Set Index
    -> Set Index
    -> NonEmpty (Set Index)
  connComps ccs@(cc :| restCcs) locs unvisited
    | Set.null locs = if Set.null unvisited
        then
          ccs
        else
          let (seed, restUnvisited) = Set.deleteFindMin unvisited
          in  connComps (Set.empty <| ccs) (Set.singleton seed) restUnvisited
    | otherwise =
        let (loc, restLocs) = Set.deleteFindMin locs
            restUnvisited = Set.delete loc unvisited
            islandState = state Map.! loc
            conn = Set.fromList $ mapMaybe
              (($ islandState) . snd)
              ( filter
                  hasBridges
                  [ (topB, topNeighbor)
                  , (rightB, rightNeighbor)
                  , (bottomB, bottomNeighbor)
                  , (leftB, leftNeighbor)
                  ]
              )
            hasBridges (dir, _) = 0 `notElem` map dir (iBridgeSets islandState)
            newCcs = Set.insert loc cc :| restCcs
            newLocs = Set.union restLocs conn Set.\\ cc
        in  connComps newCcs newLocs restUnvisited

  solveForIsland :: Island -> [State]
  solveForIsland island@(_, islandState) =
    concatMap tryBridgeSet $ iBridgeSets islandState
   where
    tryBridgeSet bridgeSet =
      [ newState
      | newStates <- narrow locs $ setBridgeSets state island [bridgeSet]
      , newState <- solveState newStates
      ]
    locs = locsFromIslandState islandState

narrow :: Set Index -> State -> [State]
narrow locs state
    -- No locations to test: return the state
  | Set.null locs = [state]
    -- The first location has no valid bridge sets: no valid state
  | null validIslandBridgeSets = []
    -- The bridge sets of the first location are all valid: test the next
    -- location
  | islandBridgeSets == validIslandBridgeSets = narrow remainingLocs state
    -- Otherwise, replace the bridge sets of the first location with valid ones
    -- and add locations potentially affected by the change to the locations to
    -- be tested, and test that
  | otherwise =
      let newLocs = locsFromIslandState islandState
      in  narrow
            (Set.union remainingLocs newLocs)
            (setBridgeSets state island validIslandBridgeSets)
 where
  (loc, remainingLocs) = Set.deleteFindMin locs
  islandState = state Map.! loc
  island = (loc, islandState)
  islandBridgeSets = iBridgeSets islandState
  validIslandBridgeSets =
      filter (noXings rightB rightXings bottomB)
    $ filter (noXings bottomB bottomXings rightB)
    $ filter (match topB topNeighbor bottomB)
    $ filter (match rightB rightNeighbor leftB)
    $ filter (match bottomB bottomNeighbor topB)
    $ filter (match leftB leftNeighbor rightB) islandBridgeSets

  match :: GetBridge -> (IslandState -> Maybe Index) -> GetBridge -> BridgeSet -> Bool
  match thisB mNeighbour otherB bridgeSet = maybe
    True
    (hasBridge (thisB bridgeSet) otherB . getBridgeSets)
    (mNeighbour islandState)

  noXings :: GetBridge -> (IslandState -> [Index]) -> GetBridge -> BridgeSet -> Bool
  noXings thisB others otherB b =
       thisB b == 0
    || all (hasNoBridge otherB . getBridgeSets) (others islandState)

  hasBridge :: Int -> GetBridge -> [BridgeSet] -> Bool
  hasBridge n dirB = (n `elem`) . map dirB

  hasNoBridge :: GetBridge -> [BridgeSet] -> Bool
  hasNoBridge = hasBridge 0

  getBridgeSets :: Index -> [BridgeSet]
  getBridgeSets = iBridgeSets . (state Map.!)

setBridgeSets :: State -> Island -> [BridgeSet] -> State
setBridgeSets state (loc, islandState) bridgeSets =
  Map.insert loc (islandState {iBridgeSets = bridgeSets}) state

locsFromIslandState :: IslandState -> Set Index
locsFromIslandState islandState =
  Set.fromList $ neighbours <> rightXings islandState <> bottomXings islandState
 where
  neighbours = mapMaybe
    ($ islandState)
    [topNeighbor, rightNeighbor, bottomNeighbor, leftNeighbor]
