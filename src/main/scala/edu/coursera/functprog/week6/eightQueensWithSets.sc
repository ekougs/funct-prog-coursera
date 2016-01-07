def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] = {
    if(k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }
  placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]): Boolean = {
  // Next queen is placed on the next available row
  val row = queens.length
  // We just have to check for the columns
  val queensWithRows = (row - 1 to 0 by -1) zip queens
  queensWithRows forall {
    case (r,c) => col != c && math.abs(col - c) != row - r
  }
}

def show(queens: List[Int]): String = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

val combinations: Set[List[Int]] = queens(4)
combinations.size

combinations map show
(queens(8) take 3 map show) mkString "\n"