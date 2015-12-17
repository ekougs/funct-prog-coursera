def sum(f: Int => Int): (Int, Int) => Int = {
  def sum(a: Int, b: Int): Int = {
    def sumIter(a: Int, acc: Int): Int =
      if (a > b) acc
      else sumIter(a + 1, acc + f(a))
    sumIter(a, 0)
  }
  sum
}

// Same than sum but using special syntax
// This syntax can be chained (a:Int, b:Int):Int
def sum1(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0 else f(a) + sum1(f)(a + 1, b)
}

def sumSquares = sum(x => x * x)

// Direct use
def cube(x: Int): Int = x * x * x
sum1(cube)(1, 10)


// Exercise
// Define product on a interval
def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1 else f(a) * product(f)(a + 1, b)
}

// Define factorial with product
def factorial(n: Int): Int = product(x => x)(1, n)

// Define a function that generalize sum and product
def mapReduce(combine: (Int, Int) => Int, zeroValue: Int)(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) zeroValue else combine(f(a), mapReduce(combine, zeroValue)(f)(a + 1, b))
}

def sumFromMapReduce:(Int => Int) => (Int, Int) => Int = mapReduce((x, y) => x + y, 0)
def productFromMapReduce:(Int => Int) => (Int, Int) => Int = mapReduce((x, y) => x * y, 1)

println(sumFromMapReduce(x => x)(1, 2) == sum1(x => x)(1, 2))
println(sumFromMapReduce(x => x)(1, 2) == sum(x => x)(1, 2))
println(productFromMapReduce(x => x)(1, 2) == product(x => x)(1, 2))