module MkSolution
  ( mkSolution
  ) where

import           Data.Foldable ( foldrM )
import qualified Data.Map as Map
import           System.Random ( randomRIO )

import           Hashi.Types
                   ( Bridges, Index, IslandC, Islands, Solution, opposite )

mkSolution :: Islands -> IO Solution
mkSolution islands = foldrM addIsland Map.empty (Map.toList islands)

addIsland :: (Index, IslandC) -> Solution -> IO Solution
addIsland (index, island) solution = do
  bridges <- mkBridges solution island
  pure $ Map.insert index bridges solution

mkBridges :: Solution -> IslandC -> IO Bridges
mkBridges solution island = do
  let connections = Map.toList island
  bridges <- traverse
    ( \(dir, index) -> case Map.lookup index solution of
        Nothing -> do
          weight <- randomRIO (1, 2)
          pure (dir, (index, weight))
        Just nextBridges -> case Map.lookup (opposite dir) nextBridges of
          Nothing -> error "Inconsistent!"
          Just (_, weight') -> pure (dir, (index, weight'))
    )
    connections
  pure $ Map.fromList bridges
