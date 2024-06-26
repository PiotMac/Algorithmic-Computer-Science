-- subtask 1
binomial  :: Int -> Int -> Int
binomial  n 0 = 1
binomial  0 k = 0
binomial  n k = binomial  (n-1) (k-1) * n `div` k

-- subtask 2
pascal :: [[Integer]]
pascal = [1] : map nextRow pascal
  where nextRow row = zipWith (+) (row ++ [0]) ([0] ++ row)

binomial2 :: Int -> Int -> Integer
binomial2 n k = (pascal !! n) !! k

-- subtask 3
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (ys, zs) = splitAt (length xs `div` 2) xs
               in merge (mergesort ys) (mergesort zs)

-- subtask 4
de :: (Int, Int) -> (Int, Int, Int)
de (a, 0) = (1, 0, a)
de (a, b) =
    let (x', y', gcd) = de (b, (a `mod` b))
        (x, y) = (y', x' - y' * (a `div` b))
    in 
        (x, y, gcd)

-- subtask 5
primeFactors :: Integer -> [Integer]
primeFactors n = factor n 2
  where
    factor n p
      | p * p > n      = [n | n > 1]
      | n `mod` p == 0 = p : factor (n `div` p) p
      | otherwise      = factor n (p + 1)

-- subtask 6
totient :: Int -> Int
totient n = length [x | x <- [1..n], gcd x n == 1]

-- subtask 7
totient2 :: Integer -> Integer
totient2 n = product [(p - 1) * p ^ (k - 1) | (p, k) <- factorize n]
    where
        factorize n = [(p, length xs) | xs@(p:_) <- group $ primeFactors n]
        group [] = []
        group (x:xs) = (x : takeWhile (==x) xs) : group (dropWhile (==x) xs)

-- subtask 8
primes :: Int -> [Int]
primes n = filter isPrime [2..n]
  where
    isPrime p = null [x | x <- [2..sqrtP], p `mod` x == 0]
      where sqrtP = floor . sqrt $ fromIntegral p

-- main
main = do
    putStrLn ("Binomial coefficient for n = 100 and k = 10: " ++ show (binomial 100 10))
    putStrLn ("Binomial coefficient (using Pascal's triangle) for n = 100 and k = 10: " ++ show (binomial2 100 10))
    putStrLn ("Merge sort of [4, 2, 1, 3, 8, 10, 17, 9, 5]: " ++ show (mergesort [4, 2, 1, 3, 8, 10, 17, 9, 5]))
    putStrLn ("Extended GCD for 12345x + 67890y = z: " ++ show (de (12345, 67890)))
    putStrLn ("Prime factors of 12345678: " ++ show (primeFactors 12345678))
    putStrLn ("Euler's Totient function for 100: " ++ show (totient 100))
    putStrLn ("Euler's Totient function using prime factors for 100: " ++ show (totient2 100))
    putStrLn ("Primes up to 100: " ++ show (primes 100))
