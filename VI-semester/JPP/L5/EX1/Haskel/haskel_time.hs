import System.CPUTime
import Text.Printf
import Data.Time

binomial  :: Int -> Int -> Int
binomial  n 0 = 1
binomial  0 k = 0
binomial  n k = binomial  (n-1) (k-1) * n `div` k

pascal :: [[Integer]]
pascal = [1] : map nextRow pascal
  where nextRow row = zipWith (+) (row ++ [0]) ([0] ++ row)

binomial2 :: Int -> Int -> Integer
binomial2 n k = (pascal !! n) !! k

primeFactors :: Integer -> [Integer]
primeFactors n = factor n 2
  where
    factor n p
      | p * p > n      = [n | n > 1]
      | n `mod` p == 0 = p : factor (n `div` p) p
      | otherwise      = factor n (p + 1)

totient :: Int -> Int
totient n = length [x | x <- [1..n], gcd x n == 1]

totient2 :: Integer -> Integer
totient2 n = product [(p - 1) * p ^ (k - 1) | (p, k) <- factorize n]
    where
        factorize n = [(p, length xs) | xs@(p:_) <- group $ primeFactors n]
        group [] = []
        group (x:xs) = (x : takeWhile (==x) xs) : group (dropWhile (==x) xs)

measureTime :: IO a -> IO (a, NominalDiffTime)
measureTime action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let diff = diffUTCTime end start
    return (result, diff)

main = do
    putStrLn $ "n;first;second"
    mapM_ (\i -> do
        -- (_, time1) <- measureTime (return (binomial i 10))
        -- (_, time2) <- measureTime (return (binomial2 i 10))
        (_, time1) <- measureTime (return (totient i))
        (_, time2) <- measureTime (return (totient2 (toInteger i)))
        putStrLn $ show i ++ ";" ++ show time1 ++ ";" ++ show time2
        ) [1000..2000]

