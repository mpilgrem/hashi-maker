{-|
Module      : Hashi.Types
Description : Types representing Hashiwokakero problems and solutions
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi.Types
  ( Index
  , Field (..)
  , isIsland
  , Problem (..)
  , BridgeSet (..)
  , GetBridge
  , IslandState (..)
  , isUncertain
  , Solution
  , State
  , Island
  , IslandC
  , Islands
  , Connection
  , ValidConnections
  , Direction (..)
  , Orientation (..)
  , Bridge
  , Bridges
  , opposite
  , orient
  , ortho
  , unconnectedIsland
  ) where

import qualified Data.Map as Map
import           Data.List.NonEmpty ( NonEmpty (..))

-- | Type synonym representing locations on the grid.
type Index = (Int, Int)

-- | Type representing fields on the puzzle grid.
data Field
  = Water
  | Island Int
  deriving (Eq)

instance Show Field where
  show Water = "."
  show (Island x) = show x

isIsland :: Field -> Bool
isIsland (Island _) = True
isIsland _          = False

-- | Type synonym representing puzzles.
data Problem = Problem
  { pWidthGrid :: Int
  , pHeightGrid :: Int
  , pGrid :: Map.Map Index Field
  }

-- | Type representing the bridges of an island.
data BridgeSet = BridgeSet
  { topB :: Int
  , rightB :: Int
  , bottomB :: Int
  , leftB :: Int
  } deriving (Eq, Show)

-- | Type synonyn representing functions obtaining a brige from the brides of an
-- Island.
type GetBridge = BridgeSet -> Int

-- | Type representing states of an island.
data IslandState = IslandState
  { iConstraint :: Int
  , topNeighbor :: Maybe Index
  , rightNeighbor :: Maybe Index
  , bottomNeighbor :: Maybe Index
  , leftNeighbor :: Maybe Index
  , rightXings :: [Index]
    -- ^ Islands whose bottom bridges cross with our right.
  , bottomXings :: [Index]
    -- ^ Islands whose right bridges cross with our bottom.
  , iBridgeSets :: [BridgeSet]
    -- ^ A list of possible bridges for the island.
  } deriving (Eq, Show)

isUncertain :: IslandState -> Bool
isUncertain islandState = length (iBridgeSets islandState) > 1

-- | Type synonym representing states of all islands.
type State = Map.Map Index IslandState

-- | Type synonym representing islands.
type Island = (Index, IslandState)

-- | Type representing cardinal directions.
data Direction = U | D | L | R deriving (Bounded, Enum, Eq, Ord, Show)

-- | Type representing orientations.
data Orientation = V | H deriving (Eq, Ord, Show)

-- | Type synonym representing islands.
type IslandC = Map.Map Direction Index

-- | Type synonym representing groups of connected islands.
type Islands = Map.Map Index IslandC

-- | Type synonym representing valid connections from the perspective of an
-- island.
type ValidConnections = Map.Map Direction (NonEmpty Index)

-- | Type synonym representing connections from the perspective of an island.
type Connection = (Direction, Index)

type Bridge = (Index, Int)

type Bridges = Map.Map Direction Bridge

type Solution = Map.Map Index Bridges

-- | Helper function yielding the opposite of a given cardinal direction.
opposite :: Direction -> Direction
opposite U = D
opposite D = U
opposite L = R
opposite R = L

-- | Helper function yielding the cardinal directory orthogonal to a cardinal
-- direction.
ortho :: Orientation -> Direction
ortho V = R
ortho H = U

-- | Helper function to orient the given cardinal direction.
orient :: Direction -> Orientation
orient U = V
orient D = V
orient L = H
orient R = H

unconnectedIsland :: IslandC
unconnectedIsland = Map.empty
