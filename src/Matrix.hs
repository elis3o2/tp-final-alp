{-|
Module      : Matrix
Description : Matrix library
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



gaussJordan :: Matrix Double -> Vec Double -> Vec Double
gaussJordan a b =
  let n     = V.length b
      aug   = V.zipWith (\row bi -> V.snoc row bi) a b
      final = foldl eliminate aug [0..n-1]
  in V.map V.last final



eliminate :: Matrix Double -> Int -> Matrix Double
eliminate mat k =
  let n        = V.length mat
      pivotRow = argMaxAbs k n mat
      mat'     = swapRows k pivotRow mat
      pivot    = getElem mat' k k
      rowK     | abs pivot < 1e-12 = error "Singular matrix"
               | otherwise         = V.map (/ pivot) (mat' V.! k)
      updateRow i row
               | i == k    = rowK
               | otherwise = let factor = row V.! k
                             in V.zipWith (-) row (V.map (* factor) rowK)
  in V.imap updateRow mat'



swapRows :: Int -> Int -> Matrix Double -> Matrix Double
swapRows i j mat = mat V.// [(i, mat V.! j), (j, mat V.! i)]



argMaxAbs :: Int -> Int -> Matrix Double -> Int
argMaxAbs k n mat =
  foldl (\best i ->
    if abs (getElem mat i k) > abs (getElem mat best k)
      then i
      else best
  ) k [k..n-1]



buildStationarySystem :: Matrix Double -> (Matrix Double, Vec Double)
buildStationarySystem p =
  let n        = V.length p
      pt       = matrixTranspose p
      iM       = identity n
      coefMat  = subtractMatrix pt iM
      onesRow  = V.replicate n 1
      a        = replaceRow (n-1) onesRow coefMat
      b        = V.generate n (\i -> if i == n-1 then 1 else 0)
  in (a, b)



buildSystem :: Matrix Double -> Int -> (Matrix Double, Vec Double)
buildSystem p target =
  let n         = V.length p
      iM        = identity n
      coefMat   = subtractMatrix iM p
      targetRow = V.generate n (\j -> if j == target then 1 else 0)
      a         = replaceRow target targetRow coefMat
      b         = unitVec n target
  in (a, b)