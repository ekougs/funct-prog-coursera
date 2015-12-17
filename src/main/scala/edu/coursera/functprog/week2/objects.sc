// Induced primary constructor
class Rational(x: Int, y: Int) {
  private val g = gcd(x, y)

  // Throws IllegalArgumentException if condition is false
  require(y > 0, "denominator must be positive")

  def numer = x / g

  def denom = y / g

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  def add(that: Rational): Rational = {
    new Rational(that.denom * numer + denom * that.numer, denom * that.denom)
  }

  def neg(): Rational = {
    new Rational(-numer, denom)
  }

  def sub(that: Rational): Rational = {
    add(that.neg())
  }

  def less(that: Rational): Boolean = {
    that.denom * numer < denom * that.numer
  }

  def max(that: Rational): Rational = {
    if (less(that)) that else this
  }

  override def toString = numer + " / " + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.sub(y).sub(z)

y.add(y)