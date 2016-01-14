object maps {
  val romanNumerals: Map[String, Int] = Map("I" -> 1, "V" -> 5, "X" -> 10)

  val capitalsOfCountry = Map("Senegal" -> "Dakar", "France" -> "Paris")

  class Polynomial(val terms0: Map[Int, Double]) {
    // Constructor with varargs
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    // Non existing exp are set to 0.
    val terms = terms0 withDefaultValue 0.0

    // An implementation with foldLeft is more efficient than this one
    def +(other: Polynomial): Polynomial = new Polynomial(terms ++ (other.terms map addTerm))

    // This one does not adjust existing polynomial but replace existing terms
    // def +(other: Polynomial): Polynomial = new Polynomial(terms ++ other.terms)

    private def addTerm(term: (Int, Double)) = {
      val (degree, coeff) = term
      degree -> (coeff + terms(degree))
    }

    override def toString: String = {
      (for ((exp, coef) <- terms.toList.sorted.reverse) yield coef + makeTerm(exp)) mkString " + "
    }

    def makeTerm(exp: Int): String = exp match {
      case 0 => ""
      case 1 => " * x"
      case y => " * x^" + y
    }
  }

}

// Retourne des Options
maps.romanNumerals get "I"
def showCapital(country: String) = maps.capitalsOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}
showCapital("Senegal")
showCapital("US")
val p1 = new maps.Polynomial(1 -> 2, 3 -> 4, 5 -> 6.2)
val p2 = new maps.Polynomial(0 -> 3, 3 -> 7)
val p3 = p1 + p2
p3.terms(7)
p3.terms(3)
