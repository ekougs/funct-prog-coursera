import edu.coursera.functprog.week3.{NonEmptySet, IntSet}

// Every type that is a subtype of Intset
def assertAllPos[S <: IntSet](set: S): S
// Every type that is a subtype of Intset and that has NonEmptySet as subtype
def assertAllPos[S >: NonEmptySet <: IntSet](set: S): S