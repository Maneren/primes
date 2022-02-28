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
  assert_eq!(primes_iter().nth(999), Some(7919));

  timeit!(
    {
      primes_iter().take(1_000).for_each(drop);
    },
    "1k primes (iter)"
  );

  timeit!(
    {
      primes_iter().nth(100_000 - 1);
    },
    "100k-th prime (iter)"
  );
}

fn primes_iter() -> impl Iterator<Item = u64> {
  let is_prime = |n: &u64| -> bool {
    if *n <= 3 {
      return *n > 1;
    }

    if n % 2 == 0 || n % 3 == 0 {
      return false;
    }

    let upper_bound = ((*n as f64).sqrt().ceil() + 1.0) as u64;

    (5_u64..upper_bound)
      .step_by(6)
      .all(|i| n % i != 0 && n % (i + 2) != 0)
  };

  (1..).filter(is_prime)
}
