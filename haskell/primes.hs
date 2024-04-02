main :: IO ()
main = do
  contents <- getContents
  let (n : inputs) = map read $ lines contents :: [Int]
  let primes_list = takeWhile (< maximum inputs) primes
  let partial_sums = drop 1 $ scanl (+) 0 $ primes_list
  let indexed_partial_sums = zip primes_list partial_sums
  let answers = map (findPartialSum indexed_partial_sums) inputs
  mapM_ print answers
 where
  findPartialSum ips n = if n < 2 then 0 else snd . last $ takeWhile (\(p, _) -> p <= n) ips

primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]
 where
  isPrime n = all (\x -> rem n x /= 0) $ takeWhile (\y -> y * y <= n) primes
