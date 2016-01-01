
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
  //  (xs zip ys).map(xy => xy._1 * xy._2).sum
  (xs zip ys).map({case (x,y) => x * y}).sum
}

def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

isPrime(4)
isPrime(19)