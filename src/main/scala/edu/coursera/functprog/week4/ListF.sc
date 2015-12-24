import edu.coursera.functprog.week3.{ListUtils, Cons, List}

object ListF {
  def apply[T](v1: T): List[T] = ListUtils.singleton(v1)

  def apply[T](v1: T, v2: T): List[T] = new Cons(v1, ListUtils.singleton(v2))
}

ListF(1)
ListF(1, 2)