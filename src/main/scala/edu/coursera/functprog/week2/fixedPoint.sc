val tolerance = .0001

def abs(x: Double): Double = if (x >= 0) x else -x

def isCloseEnough(x: Double, y: Double): Boolean = {
  abs((x - y) / x) / x < tolerance

}

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def fixedPointIter(guess: Double): Double = {
    val next = f(guess)
    if(isCloseEnough(guess, next)) next
    else fixedPointIter(next)
  }
  fixedPointIter(firstGuess)
}
fixedPoint(x => 1 + x / 2)(1)
def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def squareRootFPVersion(x: Double): Double = {
  // Does not prevent oscillation and does not converge
  // fixedPoint(y => x / y)(1.0)
  // Taking averages is good enough to make it converge
  fixedPoint(averageDamp(y => x / y))(1.0)
}

squareRootFPVersion(4)