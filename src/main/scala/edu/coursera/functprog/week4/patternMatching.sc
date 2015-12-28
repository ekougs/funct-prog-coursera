// With OO decomposition
trait ExprD {
  def eval(): Int
}

class NumberD(n: Int) extends ExprD {
  def eval(): Int = n
}

class SumD(left: ExprD, right: ExprD) extends ExprD {
  def eval(): Int = left.eval + right.eval
}

class ProdD(left: ExprD, right: ExprD) extends ExprD {
  def eval(): Int = left.eval * right.eval
}

// With pattern matching
trait ExprP {
  def precedence: Int

  def eval: Int = this match {
    case NumberP(n) => n
    case SumP(left, right) => left.eval + right.eval
    case ProdP(left, right) => left.eval * right.eval
  }
}

case class NumberP(n: Int) extends ExprP {
  override def precedence: Int = 0
}

case class SumP(left: ExprP, right: ExprP) extends ExprP {
  override def precedence: Int = 2
}

case class ProdP(left: ExprP, right: ExprP) extends ExprP {
  override def precedence: Int = 1
}


// Companion objects implicitly created by construction above
/*
object NumberP {
  def apply(n: Int) = new NumberD(n)
}

object SumP {
  def apply(left: ExprP, right: ExprP) = new SumP(left, right)
}
*/
def eval(e: ExprP): Int = e match {
  case NumberP(n) => n
  case SumP(left, right) => eval(left) + eval(right)
  case ProdP(left, right) => eval(left) * eval(right)
}

def show(expr: ExprP): String = {
  def showConsideringPrecedence(containingExpr: ExprP, containedExpr: ExprP): String = {
    def wrap(exprStr: String): String = {
      "(" + exprStr + ")"
    }
    if (containingExpr.precedence < containedExpr.precedence) wrap(show(containedExpr)) else show(containedExpr)
  }

  expr match {
    case NumberP(n) => n.toString
    case SumP(left, right) => showConsideringPrecedence(expr, left) + " + " + showConsideringPrecedence(expr, right)
    case ProdP(left, right) => showConsideringPrecedence(expr, left) + " * " + showConsideringPrecedence(expr, right)
  }
}
show(ProdP(SumP(NumberP(2), NumberP(8)), NumberP(4)))
show(ProdP(NumberP(4), SumP(NumberP(2), NumberP(8))))
show(ProdP(SumP(NumberP(4), NumberP(10)), SumP(NumberP(2), NumberP(8))))
show(SumP(ProdP(NumberP(2), NumberP(8)), NumberP(4)))