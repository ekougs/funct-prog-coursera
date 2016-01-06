// Look (i, j) integers <= to a given n such that i + j is a prime
// What is important is sequence generation use instead of iterations
def primePairs(n: Int) = {
  // This gives us combinations of (i,j) such that 1 <= j < i < n
  // We flatmap because if of the 2 "until"
  // as it would otherwise produce vector of vectors
  (1 until n).flatMap(i => {
    (1 until i) map (j => (i, j))
  }) filter (pair => isPrime(pair._1 + pair._2))
}
def isPrime(n: Int): Boolean = {
  (2 until n) forall (n % _ != 0)
}
primePairs(5)

// Prime pairs version using for
def primePairsF(n: Int) = {
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
}

primePairsF(5)

// Scalar Product using for
def scalarProductF(xs: List[Double], ys: List[Double]): Double = {
  (for ((x, y) <- xs zip ys) yield x * y).sum
}