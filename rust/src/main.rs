use std::time::Instant;

macro_rules! timeit {
  ($a:expr,$b:expr) => {
    let start = Instant::now();

    let _ = $a;

    let elapsed = start.elapsed();
    println!("{} in {elapsed:?}", $b);
  };
}

fn isqrt(n: usize) -> usize {
  (n as f64).sqrt() as usize
}

fn main() {
  // check it if is working correctly
  // 7919 is taken from wikipedia
  assert_eq!(primes_iter().nth(999), Some(7919));
  assert_eq!(primes_sieve(1_000).get(999), Some(&7919));
  assert_eq!(primes_sieve_iter().nth(999), Some(7919));
  assert_eq!(PrimeSieve::new().nth(999), Some(7919));

  println!("Values correct, timing...\n");

  const COUNT: usize = 2_000_000;

  timeit!(
    primes_iter().take(COUNT).collect::<Vec<_>>(),
    "2M primes (iter::collect)"
  );

  timeit!(
    primes_sieve_iter().take(COUNT).collect::<Vec<_>>(),
    "2M primes (iter_sieve::collect)"
  );

  timeit!(
    PrimeSieve::new().take(COUNT).collect::<Vec<_>>(),
    "2M primes (PrimeSieve::collect)"
  );

  timeit!(primes_sieve(COUNT), "2M primes (pure sieve)");
}

fn primes_iter() -> impl Iterator<Item = usize> {
  (2..).filter(|&n| match n {
    0 | 1 => false,
    2 | 3 => true,
    n if n % 2 == 0 || n % 3 == 0 => false,
    _ => (5_usize..(isqrt(n) + 1))
      .step_by(6)
      .all(|i| n % i != 0 && n % (i + 2) != 0),
  })
}

fn primes_sieve(count: usize) -> Vec<usize> {
  let mut primes = Vec::with_capacity(count);

  let mut n = 2;

  'outer: while primes.len() < count {
    let upper_bound = isqrt(n) + 1;

    for i in &primes {
      if *i > upper_bound {
        break;
      }

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

struct PrimeSieve {
  primes: Vec<usize>,
}

impl PrimeSieve {
  pub fn new() -> PrimeSieve {
    PrimeSieve { primes: Vec::new() }
  }
}

impl Iterator for PrimeSieve {
  type Item = usize;

  fn next(&mut self) -> Option<Self::Item> {
    if self.primes.is_empty() {
      self.primes.push(2);
      return Some(2);
    }

    let mut n = self.primes.last()? + 1;

    let next = loop {
      let upper_bound = isqrt(n) + 1;

      if self
        .primes
        .iter()
        .take_while(|i| **i <= upper_bound)
        .all(|i| n % i != 0)
      {
        break n;
      }

      n += 1
    };

    self.primes.push(next);
    Some(next)
  }
}

fn primes_sieve_iter() -> impl Iterator<Item = usize> {
  (1..).scan(Vec::new(), |primes, i| {
    let mut n = *primes.last().unwrap_or(&i) + 1;

    loop {
      let upper_bound = isqrt(n) + 1;

      if primes
        .iter()
        .take_while(|i| **i < upper_bound)
        .all(|i| n % i != 0)
      {
        break;
      }

      n += 1;
    }

    primes.push(n);

    Some(n)
  })
}
