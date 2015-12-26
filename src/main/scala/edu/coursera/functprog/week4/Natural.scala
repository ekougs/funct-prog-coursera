package edu.coursera.functprog.week4

trait Natural {
  def isZero: Boolean
  def predecessor: Natural
  def successor: Natural = new Successor(this)
  def +(that: Natural): Natural
  def -(that: Natural): Natural
}

object Zero extends Natural {
  def isZero: Boolean = true

  def predecessor: Natural = throw new Error("O.predecessor")

  def +(that: Natural): Natural = that

  def -(that: Natural): Natural = if(that.isZero) this else throw new Error("negative number")
}

class Successor(n: Natural) extends Natural {
  def isZero: Boolean = false

  def predecessor: Natural = n

  def +(that: Natural): Natural = new Successor(n + that)

  def -(that: Natural): Natural = if(that.isZero) this else n - that.predecessor
}
