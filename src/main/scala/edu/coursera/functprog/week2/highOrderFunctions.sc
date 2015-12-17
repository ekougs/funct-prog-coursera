def sum(f: Int => Int, a: Int, b: Int): Int=
  if (a > b) 0 else f(a) + sum(f, a + 1, b)

def id(x: Int): Int = x
def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)

def sumInts(a: Int, b: Int): Int = sum(id, a, b)
// Example of anonymous function (no name)
// Those functions can be assign to "def"
def sumCubes(a: Int, b: Int): Int = sumTailRec((x: Int) => x * x * x, a, b)
def sumFact(a: Int, b: Int): Int = sum(fact, a, b)

def sumTailRec(f: Int => Int, a: Int, b: Int): Int= {
  def sumIter(a: Int, acc: Int): Int=
    if(a > b) acc
    else sumIter(a + 1, acc + f(a))
  sumIter(a, 0)
}
