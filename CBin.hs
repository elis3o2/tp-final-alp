module CBin where

fact :: Integer -> Integer
fact 0 = 1
fact n | n < 0 = error "No definido para negativos"
       | otherwise = product [1 .. n]


factDiv :: Integer -> Integer -> Integer
factDiv n m | n < m  = error "No definido" 
            | otherwise = product [m .. n]
    


infixl 7 |*|

(|*|) :: Integer -> Integer -> Integer
n |*| k
  | k < 0 || k > n = error "No definido"
  | k == 0 || k == n = 1
  | otherwise =
      let k' = min k (n - k)
      in factDiv n (n - k') `div` fact k'