package xyz.discretezoo.core.primitives

import scala.annotation.tailrec

sealed trait Permutation {
  def max: Int
  def ofDegree(degree: Int): ActualPermutation
  def asCycles: Seq[Seq[Int]]
  def GAP: String
}

object Identity extends Permutation {
  override def max = 0
  override def ofDegree(degree: Int): ActualPermutation = ActualPermutation(Range(1, degree + 1))
  override def toString: String = "Id"
  override def asCycles: Seq[Seq[Int]] = Seq()
  override def GAP: String = "()"
}

case class ActualPermutation(permutation: Seq[Int]) extends Permutation {
  require(permutation.forall(_ > 0), ActualPermutation.onlyPositive)
  require(permutation.isEmpty || permutation.toSet == Range(1, permutation.max + 1).toSet, ActualPermutation.bijection)

  override def max: Int = permutation.max

  override def ofDegree(degree: Int): ActualPermutation = {
    ActualPermutation(permutation ++ Range(permutation.length + 1, degree + 1))
  }

  override def toString: String = s"Permutation(${permutation.mkString(", ")})"

  override def asCycles: Seq[Seq[Int]] = {
    val map = permutation.zipWithIndex.map(t => (t._2 + 1, t._1)).toMap

    @tailrec def getCycle(accumulated: Seq[Int]): Seq[Int] = {
      val maybeNext = map(accumulated.last)
      if (accumulated.head == maybeNext) accumulated
      else getCycle(accumulated :+ maybeNext)
    }

    @tailrec def getCycles(accumulated: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      val used = accumulated.flatten.toSet
      val remaining = map.values.toSet.diff(used)
      if (remaining.isEmpty) accumulated
      else getCycles(accumulated :+ getCycle(Seq(remaining.min)))
    }

    getCycles(Seq()).filter(_.length > 1)
  }

  def isIdentity: Boolean = permutation.zipWithIndex.forall(t => t._1 == t._2 + 1)

  override def GAP: String = asCycles.map(_.mkString("(", ",", ")")).mkString

}

object ActualPermutation {

  protected val onlyPositive = "Points should only be positive (non-zero) integers."
  protected val bijection = "The sequence needs to represent a permutation in one-line notation."

  private def arePositivePoints(cycles: Seq[Seq[Int]]): Boolean = cycles.flatMap(_.toSet).forall(_ > 0)

  def fromCycleNotation(cycles: Seq[Seq[Int]]): ActualPermutation = {
    require(arePositivePoints(cycles), onlyPositive)
    ActualPermutation(mapToOneLine(cyclesToMap(cycles)))
  }

  // Cycles to map
  // Takes a list of cycles (representing a permutation) and outputs a corresponding map
  // Ignores fixed points
  def cyclesToMap(cycles: Seq[Seq[Int]]): Map[Int, Int] = {
    require(arePositivePoints(cycles), onlyPositive)
    cycles.flatMap(cycle => {
      cycle.sliding(2).map(pair => (pair.head, pair.last)).toSeq :+ (cycle.last, cycle.head)
    }).toMap
  }

  def mapToOneLine(map: Map[Int, Int]): Seq[Int] = {
    require(map.flatMap(m => Set(m._1, m._2)).forall(_ > 0), onlyPositive)
    if (map.isEmpty) Seq[Int]()
    else Range(1, map.values.max + 1).map(x => map.getOrElse(x, x))
  }

}

//val setSize = this.permutation.size
//
//require(this.permutation.keySet == this.permutation.values.toSet, "The argument is not a permutation")
//require(this.permutation.keySet == Range(0, setSize).toSet, "A permutation on " + setSize + " elements must be defined on the set {0, ..., " + (setSize-1) + "}.")
//
//def apply(_1: Int) = permutation(_1)