{-|
Module      : Matrix
Description : Matrix operations library: multiplication, exponentiation and linear systems.
-}
module Matrix where

import Common
import qualified Data.Vector as V

getElem :: Matrix a -> Int -> Int -> a
getElem mat i j = (mat V.! i) V.! j


getCol :: Matrix a -> Int -> Vec a
getCol m j = V.map (\row -> row V.! j) m


matrixTranspose :: Matrix a -> Matrix a
matrixTranspose m =
  let cols = V.length (m V.! 0)
  in V.generate cols (getCol m)


dot :: Vec Double -> Vec Double -> Double
dot v1 v2 = V.sum (V.zipWith (*) v1 v2)


identity :: Int -> Matrix Double
identity n = V.generate n $ \i -> V.generate n $ \j -> if i == j then 1 else 0


matrixMul :: Matrix Double -> Matrix Double -> Matrix Double
matrixMul a b =
  let colsB = matrixTranspose b
  in V.map (\rowA -> V.map (dot rowA) colsB) a


matrixEx :: Matrix Double -> Int -> Matrix Double
matrixEx m 0 = identity (V.length m)
matrixEx m n | even n    = let half = matrixEx m (n `div` 2)
                            in matrixMul half half
             | otherwise = matrixMul m (matrixEx m (n - 1))


subtractMatrix :: Matrix Double -> Matrix Double -> Matrix Double
subtractMatrix a b = V.zipWith (V.zipWith (-)) a b



replaceRow :: Int -> Vec Double -> Matrix Double -> Matrix Double
replaceRow i newRow mat = mat V.// [(i, newRow)]


zeroVec :: Int -> Vec Double
zeroVec n = V.replicate n 0


unitVec :: Int -> Int -> Vec Double
unitVec n j = V.generate n (\i -> if i == j then 1 else 0)



-- Solves Ax = b via Gauss-Jordan on the augmented matrix [A|b]
gaussJordan :: Matrix Double -> Vec Double -> Vec Double
gaussJordan a b =
  let n     = V.length b
      -- Builds [A|b] by appending each bi to the end of its row
      aug   = V.zipWith (\row bi -> V.snoc row bi) a b
      -- Applies elimination column by column
      final = foldl eliminate aug [0..n-1]
  -- The solution sits in the last column of each row
  in V.map V.last final



-- Performs one Gauss-Jordan elimination step for column k
eliminate :: Matrix Double -> Int -> Matrix Double
eliminate mat k =
  let n        = V.length mat
      -- Finds the row with the largest absolute value in column k (partial pivoting)
      pivotRow = argMaxAbs k n mat
      -- Moves that row to position k
      mat'     = swapRows k pivotRow mat
      pivot    = getElem mat' k k
      -- Normalizes row k so the pivot becomes 1
      rowK     | abs pivot < 1e-12 = error "Singular matrix"
               | otherwise         = V.map (/ pivot) (mat' V.! k)
      -- Eliminates the column k coefficient from every other row
      updateRow i row
        | i == k    = rowK
        | otherwise = let factor = row V.! k
                      in V.zipWith (-) row (V.map (* factor) rowK)
  in V.imap updateRow mat'


swapRows :: Int -> Int -> Matrix Double -> Matrix Double
swapRows i j mat = mat V.// [(i, mat V.! j), (j, mat V.! i)]


-- Returns the index of the row with the largest absolute value in column k
-- only looking at the unprocessed portion (rows k..n-1)
argMaxAbs :: Int -> Int -> Matrix Double -> Int
argMaxAbs k n mat =
  foldl (\best i ->
    if abs (getElem mat i k) > abs (getElem mat best k)
    then i
    else best
  ) k [k..n-1]


-- Builds the system (A, b) whose solution is the stationary distribution pi
-- satisfying pi * P = pi and sum(pi) = 1
buildStationarySystem :: Matrix Double -> (Matrix Double, Vec Double)
buildStationarySystem p =
  let n        = V.length p
      pt       = matrixTranspose p
      iM       = identity n
      -- P^T - I: each solution satisfies (P^T - I)x = 0
      coefMat  = subtractMatrix pt iM
      -- Replaces the last equation with the normalization constraint sum(pi_i) = 1
      onesRow  = V.replicate n 1
      a        = replaceRow (n-1) onesRow coefMat
      b        = V.generate n (\i -> if i == n-1 then 1 else 0)
  in (a, b)



-- Builds the system (A, b) for computing first passage times to state target
-- Solves (I - P)x = e_target with the target row fixed to x_target = 1
buildSystem :: Matrix Double -> Int -> (Matrix Double, Vec Double)
buildSystem p target =
  let n         = V.length p
      iM        = identity n
      -- (I - P): solution x_i represents the expected time from i to target
      coefMat   = subtractMatrix iM p
      -- Fixes the target state equation: x_target = 1
      targetRow = V.generate n (\j -> if j == target then 1 else 0)
      a         = replaceRow target targetRow coefMat
      b         = unitVec n target
  in (a, b)