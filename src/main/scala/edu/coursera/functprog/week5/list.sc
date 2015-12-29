def removeAt[T](n: Int, xs: List[T]): List[T]= (xs take n) ::: (xs drop n + 1)

removeAt(1, List('a', 'b', 'c', 'd'))