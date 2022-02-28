use std::time::Instant;

fn main() {
  let start = Instant::now();

  (0..).filter(is_prime).take(1000).for_each(drop);
  // .collect::<Vec<_>>();

  let elapsed = start.elapsed();

  println!("1k primes in {elapsed:?}");
}

fn is_prime(n: &usize) -> bool {
  if *n <= 3 {
    return *n > 1;
  }

  if n % 2 == 0 || n % 3 == 0 {
    return false;
  }

  !(5_usize..)
    .step_by(6)
    .take_while(|&i| i.pow(2) <= *n)
    .any(|i| n % i == 0 || n % (i + 2) == 0)
}
