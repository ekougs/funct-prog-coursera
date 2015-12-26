import edu.coursera.functprog.week3.{EmptySet, NonEmptySet}

val t1 = new NonEmptySet(4, new EmptySet, new EmptySet)
val t2 = t1.incl(3)