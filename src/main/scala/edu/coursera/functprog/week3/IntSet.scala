package edu.coursera.functprog.week3

abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(other: IntSet): IntSet
}

class EmptySet extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)

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
