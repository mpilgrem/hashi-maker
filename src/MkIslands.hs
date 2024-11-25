module MkIslands
  ( mkIslands
  ) where

import           Data.List.NonEmpty ( NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Maybe ( isNothing )
import           System.Random ( randomRIO )

import           Hashi.Types
                   ( Connection, Direction (..), Index, Islands
                   , Orientation (..), ValidConnections, opposite, orient, ortho
                   , unconnectedIsland
                   )

order :: Index -> Connection -> (Index, Index)
order start (U, dest) = (start, dest)
order start (D, dest) = (dest, start)
order start (L, dest) = (dest, start)
order start (R, dest) = (start, dest)

mkIslands :: Int -> Int -> IO Islands
mkIslands width height = do
  seed <- pickSeed
  growIslands [seed] $ Map.singleton seed unconnectedIsland
 where
  pickSeed :: IO Index
  pickSeed = do
    col <- randomRIO (0, width - 1)
    row <- randomRIO (0, height - 1)
    pure (col, row)

  growIslands :: [Index] -> Islands -> IO Islands
  growIslands [] islands = pure islands
  growIslands seeds@(thisIndex@(col, row) : otherSeeds) islands = do
    mConnection <- pickNext validConnections
    maybe
      (growIslands otherSeeds islands)
      ( \connection@(_, nextIndex) ->
          growIslands (nextIndex : seeds) (addIsland connection)
      )
     mConnection
   where
    mIsland = Map.lookup thisIndex islands

    pickNext :: ValidConnections -> IO (Maybe Connection)
    pickNext vc = do
      mIndices <- randomKeyValue vc
      maybe
        (pure Nothing)
        ( \(dir, indices) -> do
            i <- randomRIO (0, NE.length indices - 1)
            pure $ Just (dir, indices NE.!! i)
        )
        mIndices

    validConnections :: ValidConnections
    validConnections = foldr go Map.empty [U .. R]
     where
      go :: Direction -> ValidConnections -> ValidConnections
      go dir acc = case validIndices of
        [] -> acc
        (i : is) -> Map.insert
          dir
          (i :| is)
          acc
       where
        validIndices = filter
          (\index -> isValid index && not (hasCrossing index))
          possIndicies

        possIndicies = if validDir
          then case dir of
            D -> takeWhile isFree [(col, r') | r' <- [row - 2, row - 3 .. 0]]
            U -> takeWhile isFree [(col, r') | r' <- [row + 2 .. height - 1]]
            L -> takeWhile isFree [(c', row) | c' <- [col - 2, col - 3 .. 0]]
            R -> takeWhile isFree [(c', row) | c' <- [col + 2 .. width - 1]]
          else []

        validDir = maybe False (isNothing . Map.lookup dir) mIsland

        isFree index = isNothing (Map.lookup index islands)

        isValid (c', r') =
             ((c' == 0) || isNothing (Map.lookup (c' - 1, r') islands))
          && ((r' == 0) || isNothing (Map.lookup (c', r' - 1) islands))
          && ((c' == width - 1) || isNothing (Map.lookup (c' + 1, r') islands))
          && ((r' == height - 1) || isNothing (Map.lookup (c', r' + 1) islands))

        hasCrossing :: Index -> Bool
        hasCrossing index = anyWithKey isCrossing islands
         where
          connection = (dir, index)
          nextOrient = orient dir
          orthoDir = ortho nextOrient
          ((xs, ys), (xe, ye)) = order thisIndex connection
          isCrossing (xs', ys') island = maybe
            False
            ( \(xe', ye') -> case nextOrient of
                V ->
                     (ys' > ys && ys' < ye && xs' <= xs && xe' >= xs)
                  || ((ys' == ys || ys' == ye) && xs' < xs && xe' > xs)
                H ->
                     (xs' > xs && xs' < xe && ys' <= ys && ye' >= ys)
                  || ((xs' == xs || xs' == xe) && ys' < ys && ye' > ys)
            )
            (Map.lookup orthoDir island)

    addIsland :: Connection -> Islands
    addIsland connection@(dir, index) = Map.insert
      index
      (Map.singleton (opposite dir) thisIndex)
      (updateIslands connection)

    updateIslands :: Connection -> Islands
    updateIslands (dir, index) = maybe
      (error "No island!")
      ( \island -> Map.insert
          thisIndex
          (Map.insert dir index island)
          islands
      )
      mIsland

anyWithKey :: (k -> a -> Bool) -> Map.Map k a -> Bool
anyWithKey p = Map.foldrWithKey p' False
 where
  p' k a acc = acc || p k a

randomKeyValue :: Map.Map k a -> IO (Maybe (k, a))
randomKeyValue m
  | Map.null m = pure Nothing
  | otherwise = do
      let l = Map.toList m
      i <- randomRIO (0, length l - 1)
      pure $ Just (l !! i)
