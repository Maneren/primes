import Control.Exception
import Data.Array.Unboxed
import Data.List.Ordered
import System.TimeIt
import Text.Printf

n :: Int
n = 1000000

main :: IO ()
main =
  do
    putStrLn "Start"
    timeIt $ printf "primes_naive: %d\n" $ sum $ takeWhile (< n) primes_naive
    timeIt $ printf "primes_recursive: %d\n" $ sum $ takeWhile (< n) primes_recursive
    timeIt $ printf "primes_recursive_inline: %d\n" $ sum $ takeWhile (< n) primes_recursive_inline
    -- extremely slow
    -- timeIt $ printf "primes_naive_sieve: %d\n" $ sum $ takeWhile (< n) primes_naive_sieve
    timeIt $ printf "primes_recursive_sieve: %d\n" $ sum $ primes_recursive_sieve n
    timeIt $ printf "primes_guarded_sieve: %d\n" $ sum $ primes_guarded_sieve n
    timeIt $ printf "primes_array_sieve: %d\n" $ sum $ primes_array_sieve n
    timeIt $ printf "primes_synchronized: %d\n" $ sum $ takeWhile (< n) primes_synchronized
    timeIt $ printf "primes_linear_merging: %d\n" $ sum $ takeWhile (< n) primes_linear_merging
    putStrLn "End"

primes_naive :: [Int]
primes_naive = 2 : filter isPrime [3, 5 ..]
 where
  isPrime n = and [rem n x > 0 | x <- takeWhile (\y -> y * y <= n) [2 ..]]

primes_recursive :: [Int]
primes_recursive = 2 : filter isPrime [3, 5 ..]
 where
  isPrime n = all (\x -> rem n x /= 0) $ takeWhile (\y -> y * y <= n) primes_recursive

primes_recursive_inline :: [Int]
primes_recursive_inline = 2 : [i | i <- [3, 5 ..], and [rem i p > 0 | p <- takeWhile (\p -> p ^ 2 <= i) primes_recursive_inline]]

primes_naive_sieve :: [Int]
primes_naive_sieve = sieve [2 ..]
 where
  sieve (p : xs) = p : sieve [x | x <- xs, rem x p > 0]

primes_recursive_sieve :: Int -> [Int]
primes_recursive_sieve m = sieve [2 .. m]
 where
  sieve :: [Int] -> [Int]
  sieve [] = []
  sieve (p : xs) = p : sieve (xs `minus` [p * p, p * p + p .. m])

primes_guarded_sieve :: Int -> [Int]
primes_guarded_sieve m = 2 : sieve [3, 5 .. m]
 where
  sieve (prime : xs)
    | prime * prime > m = prime : xs
    | otherwise = prime : sieve (xs `minus` map (prime *) [prime, prime + 2 ..])

primes_array_sieve :: Int -> [Int]
primes_array_sieve m =
  sieve 3 (array (3, m) [(i, odd i) | i <- [3 .. m]])
 where
  sieve :: Int -> UArray Int Bool -> [Int]
  sieve p a
    | p * p > m = 2 : [i | (i, True) <- assocs a]
    | a ! p = sieve (p + 2) $ a // [(i, False) | i <- [p * p, p * p + 2 * p .. m]]
    | otherwise = sieve (p + 2) a

primes_synchronized :: [Int]
primes_synchronized = 2 : sieve [3 ..] [[p * p, p * p + p ..] | p <- primes_synchronized]
 where
  sieve (x : xs) multiples@((primeMultiple : cs) : rest)
    | x < primeMultiple = x : sieve xs multiples
    | otherwise = sieve (minus xs cs) rest

primes_linear_merging :: [Int]
primes_linear_merging =
  2
    : minus
      [3 ..]
      ( foldr
          (\p r -> p * p : union [p * p + p, p * p + 2 * p ..] r)
          []
          primes_linear_merging
      )
