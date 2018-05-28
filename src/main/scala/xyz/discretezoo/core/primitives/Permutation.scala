package xyz.discretezoo.core.primitives

case class Permutation(permutation: Seq[Int]) {
  require(permutation.forall(_ > 0), Permutation.onlyPositive)

  def degree: Int = permutation.length

}

object Permutation {

  protected val onlyPositive = "Points should only be positive (non-zero) integers."

  def fromCycleNotation(cycles: Seq[Seq[Int]]): Permutation = {
    require(cycles.flatMap(_.toSet).forall(_ > 0), onlyPositive)
    println(cycles)
    Permutation(mapToOneLine(cyclesToMap(cycles)))
  }

  private def cyclesToMap(cycles: Seq[Seq[Int]]): Map[Int, Int] = {
    require(cycles.flatMap(_.toSet).forall(_ > 0), onlyPositive)
    val movedPoints = cycles.flatten.toSet
    val fixedPoints = Range(1, movedPoints.max + 1).toSet.diff(movedPoints)
    val fixedPointsMap = fixedPoints.map((x: Int) => (x, x)).toSeq
    cycles.flatMap(cycle => {
      cycle.sliding(2).map(pair => (pair.head, pair.last)).toSeq ++ fixedPointsMap :+ (cycle.last, cycle.head)
    }).toMap
  }

  private def mapToOneLine(map: Map[Int, Int]): Seq[Int] = {
    val pointSet = map.flatMap(mapping => Set(mapping._1, mapping._2))
    require(pointSet.forall(_ > 0), onlyPositive)
    val degree = pointSet.max
    Range(1, degree + 1).map(x => map.getOrElse(x, x))
  }

}
