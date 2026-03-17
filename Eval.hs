fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

factDiv :: Int -> Int -> Int
factDiv n m = if n > m
              then n * factDiv (n-1) m
              else n

cofBin :: Int -> Int -> Int
cofBin n k = factDiv n (n-k) `div` fact k

getProb :: Int -> Expr -> Float
getProb k (Bin n p) = fromIntegral (cofBin n k) * (p ^ k) * ((1 - p) ^ (n - k))
getProb k (Poiss l) = (exp ^ (- l)) * (l ^ k) / fromIntegral (fact k)
getProb k (Geo n)   = ()