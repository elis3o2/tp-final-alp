{-|
Module      : Simulation
Description : Random simulation of paths in Markov chains.
-}
module Simulation where

import Common
import Monad
import qualified Data.Vector as V

-- Simulates one step from state i using the transition matrix row as a probability distribution
-- Draws a uniform random number and picks the next state accordingly
createStep :: MonadProb m => Matrix Double -> Int -> m Int
createStep mat i = do
  let row = mat V.! i
  p <- getRandom
  return (pick p row 0 0)


-- Selects the next state by finding where the uniform sample p falls in the cumulative distribution
-- Walks the row accumulating probabilities until the bucket containing p is found
pick :: Double -> V.Vector Double -> Double -> Int -> Int
pick p row acc j | p <= acc + row V.! j = j
                 | otherwise = pick p row (acc + row V.! j) (j + 1)


-- Simulates a Markov chain path of length n starting from state start
-- Returns the full sequence of visited states including the initial one
createPathInit :: MonadProb m => Matrix Double -> Int -> Int -> m [Int]
createPathInit mat start n = go start n
  where
    go current 0     = return [current]
    go current steps = do
      next <- createStep mat current
      rest <- go next (steps - 1)
      return (current : rest)



doSimulFromName :: MonadProb m => Markov -> Name -> Int -> m Path
doSimulFromName (Mk names mat) n i = do
  let start = indexOf names n
  p <- createPathInit mat start i
  return (V.fromList (map (names V.!) p))
  

  
doSimulFromVec :: MonadProb m => Markov -> Vec Double -> Int -> m Path
doSimulFromVec (Mk names mat) v i = do
  p0 <- getRandom
  let start = pick p0 v 0 0
  p <- createPathInit mat start i
  return (V.fromList (map (names V.!) p))