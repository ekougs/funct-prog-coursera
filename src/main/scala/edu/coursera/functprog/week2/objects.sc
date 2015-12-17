// Induced primary constructor
class Rational(x: Int, y: Int) {
  private val g = gcd(x, y)

  // Throws IllegalArgumentException if condition is false
  require(y > 0, "denominator " + y + " must be positive")

  def numer = x / g

  def denom = y / g

  def this(x: Int) = this(x, 1)

  private def abs(x: Int): Int = if (x >= 0) x else -x

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) abs(a) else gcd(b, a % b)
  }

  def add(that: Rational): Rational = {
    new Rational(that.denom * numer + denom * that.numer, denom * that.denom)
  }

  // Using operators as method names
  def +(that: Rational): Rational = add(that)

  def neg(): Rational = {
    new Rational(-numer, denom)
  }

  def sub(that: Rational): Rational = {
    add(that.neg())
  }

  def -(that:Rational): Rational = this + -that

  // To use minus as unary operator
  def unary_- : Rational = new Rational(-numer, denom)

  def less(that: Rational): Boolean = {
    that.denom * numer < denom * that.numer
  }

  // Using operators as method names
  def <(that: Rational): Boolean = less(that)

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
// Infix notation
y add y
y + y
y - y