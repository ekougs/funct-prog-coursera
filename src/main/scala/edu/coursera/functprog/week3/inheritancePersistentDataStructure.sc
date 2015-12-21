abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(other: IntSet): IntSet
}

// Singleton
object EmptySet extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmptySet(x, EmptySet, EmptySet)

  def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmptySet(elt: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elt) left contains x
    else if (x > elt) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elt) new NonEmptySet(elt, left incl x, right)
    else if (x > elt) new NonEmptySet(elt, left, right incl x)
    else this
  }

  def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elt
  }

  override def toString = "{" + left + elt + right + "}"
}

val t1 = new NonEmptySet(4, EmptySet, EmptySet)
val t2 = t1.incl(3)