module Markov where

import Common
import qualified Data.Vector as V
import Matrix



getNames :: Markov -> Path
getNames (Mk n _) = n 

getMatrix :: Markov -> Matrix Double
getMatrix (Mk _ m) = m


makeMatriz' :: [Name] -> [NodeVal] -> [[Double]]
makeMatriz' _ [] = []
makeMatriz' names (N n:nodes) = makerow names n : makeMatriz' names nodes
                              where
                                value _ [] = 0
                                value m ((s,d):ss) | m == s    = d
                                                   | otherwise = value m ss
                                makerow [] _ = []
                                makerow (m:ms) node = value m node : makerow ms node



makeMatriz :: [Name] -> [NodeVal] -> Matrix Double
makeMatriz names nodes = V.fromList (map V.fromList (makeMatriz' names nodes))



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
