package xyz.discretezoo.core.primitives

sealed trait Permutation {
  def max: Int
  def ofDegree(degree: Int): ActualPermutation
}

object Identity extends Permutation {
  override def max = 0
  override def ofDegree(degree: Int): ActualPermutation = ActualPermutation(Range(1, degree + 1))
  override def toString: String = "Id"
}

case class ActualPermutation(permutation: Seq[Int]) extends Permutation {
  require(permutation.forall(_ > 0), ActualPermutation.onlyPositive)
  require(permutation.isEmpty || permutation.toSet == Range(1, permutation.max + 1).toSet, ActualPermutation.bijection)

  override def max: Int = permutation.max

  override def ofDegree(degree: Int): ActualPermutation = {
    ActualPermutation(permutation ++ Range(permutation.length + 1, degree + 1))
  }

  override def toString: String = s"Permutation(${permutation.mkString(", ")})"
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
