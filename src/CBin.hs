{-|
Module      : CBin
Description : Factorial and binomial coefficient
-}
module CBin (fact, (|*|)) where

fact :: Integer -> Integer
fact 0 = 1
fact n | n < 0 = error "Not definded"
       | otherwise = product [1 .. n]


factDiv :: Integer -> Integer -> Integer
factDiv n m | n < m  = error "Not definded"
            | otherwise = product [m + 1 .. n]    


infixl 7 |*|

(|*|) :: Integer -> Integer -> Integer
n |*| k
  | k < 0 || k > n = error "Not definded"
  | k == 0 || k == n = 1
  | otherwise =
      let k' = min k (n - k)
      in factDiv n (n - k') `div` fact k'