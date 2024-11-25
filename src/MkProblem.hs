module MkProblem
  ( mkProblem
  , showProblem
  ) where

import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe )

import           Hashi.Types
                   ( Bridge, Bridges, Field (..), Problem (..), Solution )

mkProblem :: Int -> Int -> Solution -> Problem
mkProblem width height solution = Problem
  { pWidthGrid = width
  , pHeightGrid = height
  , pGrid = Map.map toField solution
  }

toField :: Bridges -> Field
toField bridges
  | Map.null bridges = Water
  | otherwise =
      Island $ Map.foldr (\bridge acc -> toWeight bridge + acc) 0 bridges

toWeight :: Bridge -> Int
toWeight (_, weight) = weight

showProblem :: Problem -> String
showProblem p = L.intercalate "\n"
  [
    concat [ show $ fromMaybe Water (Map.lookup (col, row) (pGrid p))
    | col <- [0 .. pWidthGrid p - 1]
    ]
  | row <- [0 .. pHeightGrid p - 1]
  ]
