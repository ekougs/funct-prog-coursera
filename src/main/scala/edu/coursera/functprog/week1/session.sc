def abs(x: Double): Double = if (x >= 0) x else -x

// Now only sqrt is visible in namespace
// Called a block
// Last instruction is returned
def sqrt(x: Double): Double = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess, x))

  def isGoodEnough(guess: Double): Boolean =
  // Pour abs(guess * guess - x) < 0.0001
  // si le nombre qu'on veut trouver est plus petit que notre difference,
  // ça ne marche pas car on va arreter l'algorithme avant de trouver la valeur
  // Et pour des nombres tres larges (1e60) la difference peut etre
  // tres eloignee du delta conduisant a tourner indefiniment
    abs(guess * guess - x) / x < 0.0001

  def improve(guess: Double, x: Double): Double = (guess + x / guess) / 2
  sqrtIter(1.0)
}
println(sqrt(4))
println(sqrt(2))
println(sqrt(1e-6))
println(sqrt(1e60))