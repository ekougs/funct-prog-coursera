def removeAt[T](n: Int, xs: List[T]): List[T]= (xs take n) ::: (xs drop n + 1)

removeAt(1, List('a', 'b', 'c', 'd'))

def squareListPtrnMtchImpl(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y * y :: squareListPtrnMtchImpl(ys)
}

def squareListMapImpl(xs: List[Int]): List[Int] = xs map (x => x * x)

val fiboUnordered: List[Int] = List(8, 3, 5, 2, 1, 1, 13)
squareListPtrnMtchImpl(fiboUnordered)
squareListMapImpl(fiboUnordered)

val evenNumbers: (Int) => Boolean = x => x % 2 == 0
fiboUnordered filter evenNumbers
fiboUnordered filterNot evenNumbers
fiboUnordered partition evenNumbers

fiboUnordered takeWhile evenNumbers
fiboUnordered dropWhile evenNumbers
fiboUnordered span evenNumbers

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (consecutiveDuplicates, restOfList) = xs span (y => y == x)
    consecutiveDuplicates :: pack(restOfList)
}

val letters = List("a", "a", "a", "b", "c", "c", "a")
pack(letters)

def encode[T](xs: List[T]) = xs match {
  case Nil => Nil
  case x :: xs1 =>
    pack(xs) map (ys => (ys.head, ys.length))
}

encode(letters)

