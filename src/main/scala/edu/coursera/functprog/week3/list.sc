import edu.coursera.functprog.week3.{Cons, List, ListUtils}

// Si le type est implicite pas besoin de le préciser
ListUtils.singleton(1)
ListUtils.singleton(true)

def get[T](position: Int, list: List[T]): T = {
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (position == 0) list.head
  else get(position - 1, list.tail)
}
val list: List[Int] = new Cons(1, new Cons(2, new Cons(3, ListUtils.singleton(4))))
get(2, list)
get(5, list)
