use std::time::Instant;

macro_rules! timeit {
  ($a:block,$b:expr) => {
    let start = Instant::now();

    $a

    let elapsed = start.elapsed();
    println!("{} in {elapsed:?}", $b);
  };
}

fn main() {
  // check it if is working correctly
  // 7919 is taken from wikipedia
  assert_eq!(primes_iter().nth(999), Some(7919));
  assert_eq!(primes_sieve(1_000).get(999), Some(&7919_usize));

  timeit!(
    {
      primes_iter().take(10_000).for_each(drop);
    },
    "10k primes (iter)"
  );

  timeit!(
    {
      primes_iter().nth(9999);
    },
    "10k-th prime (iter)"
  );

  timeit!(
    {
      primes_sieve(10_000);
    },
    "10k primes (sieve)"
  );
}

fn is_prime(n: &usize) -> bool {
  if *n <= 3 {
    return *n > 1;
  }

  if n % 2 == 0 || n % 3 == 0 {
    return false;
  }

  let upper_bound = ((*n as f64).sqrt().ceil() + 1.0) as usize;

  (5_usize..upper_bound)
    .step_by(6)
    .all(|i| n % i != 0 && n % (i + 2) != 0)
}
fn primes_iter() -> impl Iterator<Item = usize> {
  (2..).filter(is_prime)
}

fn primes_sieve(count: usize) -> Vec<usize> {
  let mut primes = Vec::with_capacity(count);

  let mut n = 2;

  'outer: while primes.len() < count {
    for i in &primes {
      if n % i == 0 {
        n += 1;
        continue 'outer;
      }
    }

    primes.push(n);

    n += 1
  }

  primes
}
