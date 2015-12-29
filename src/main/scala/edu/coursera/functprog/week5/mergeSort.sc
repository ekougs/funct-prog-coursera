import math.Ordering

// implicit keyword make the compiler guess the right value for ordering
def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def mergeSorted(xs: List[T], ys: List[T]): List[T] = {
      (xs, ys) match {
        case (Nil, ys1) => ys
        case (xs1, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: mergeSorted(xs1, ys)
          else y :: mergeSorted(xs, ys1)
      }
    }
    // This one returns 2 sub lists
    val (fst, snd) = xs splitAt n
    // ord is used as implicit parameter
    mergeSorted(msort(fst), msort(snd))
  }
}

val fiboUnordered = List(8, 3, 5, 2, 1, 1, 13)
msort(fiboUnordered)(Ordering.Int)

val fruits = List("pineapple", "apple", "orange", "banana")
msort(fruits)