module Main
  ( main
  , mkHashi
  ) where

import qualified Data.Map as Map
import           System.Environment ( getArgs )

import           MkIslands ( mkIslands )
import           MkSolution ( mkSolution )
import           MkProblem ( mkProblem, showProblem )
import           Hashi ( solveProblem )

main :: IO ()
main = do
  args <- getArgs
  let width = read $ args !! 0 :: Int
      height = read $ args !! 1 :: Int
      nIslands = read $ args !! 2 :: Int
  mkHashi width height nIslands

mkHashi :: Int -> Int -> Int -> IO ()
mkHashi width height nIslands = go 1
 where
  go :: Int -> IO ()
  go c = do
    m <- mkIslands width height
    if Map.size m >= nIslands
      then do
        s <- mkSolution m
        let p = mkProblem width height s
        putStrLn $ showProblem p
        putStr "\n"
        putStrLn $ "After " <> show c <> " iterations."
        case solveProblem p of
          [] -> error "The impossible happened! No solution."
          [_] -> putStrLn "Unique solution!"
          _ -> do
            putStrLn "No unique solution! Continuing..."
            go (c + 1)
      else go (c + 1)
