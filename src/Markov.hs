{-|
Module      : Markov
Description : Markov chain operations: transition matrix, path probability and stationary distribution.
-}

module Markov
  ( getNames, getMatrix, makeMatrix
  , getProbSteps, getProbPath, getProbHit
  , getStationary
  ) where
  

import Common
import qualified Data.Vector as V
import Matrix



getNames :: Markov -> Path
getNames (Mk n _) = n 

getMatrix :: Markov -> Matrix Double
getMatrix (Mk _ m) = m


makeMatrix' :: [Name] -> [NodeVal] -> [[Double]]
makeMatrix' _ [] = []
makeMatrix' names (N n:nodes) = makerow names n : makeMatrix' names nodes
                              where
                                value _ [] = 0
                                value m ((s,d):ss) | m == s    = d
                                                   | otherwise = value m ss
                                makerow [] _ = []
                                makerow (m:ms) node = value m node : makerow ms node



makeMatrix :: [Name] -> [NodeVal] -> Matrix Double
makeMatrix names nodes = V.fromList (map V.fromList (makeMatrix' names nodes))



getProbSteps :: Markov -> Name -> Name -> Int -> Double
getProbSteps (Mk names ma) n m k = let i = indexOf names n
                                       j = indexOf names m
                                       mat = matrixEx ma k
                                    in getElem mat i j 



getProbPath :: Markov -> Path -> Double
getProbPath (Mk names mat) c =
  let idxs = V.toList $ V.map (\x -> indexOf names x) c
  in product [ getElem mat i j | (i,j) <- zip idxs (tail idxs) ]



getStationary :: Markov -> Vec Double
getStationary (Mk _ m ) = let (a, b) = buildStationarySystem m 
                          in gaussJordan a b




getProbHit :: Markov -> Name -> Name -> Double
getProbHit (Mk names mat) from to =
  let target = indexOf names to
      start  = indexOf names from

      (a, b) = buildSystem mat target
      h      = gaussJordan a b

  in h V.! start
