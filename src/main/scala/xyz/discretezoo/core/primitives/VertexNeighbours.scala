package xyz.discretezoo.core.primitives

import scala.collection.mutable

sealed trait VertexNeighbours[T <: VertexNeighbours[T]] {

  val neighbours: mutable.Set[T] = mutable.Set[T]()

  def degree: Int = neighbours.size
  def addNeighbour(neighbour: T): Unit = neighbours += neighbour
  def isAdjacent(vertex: T): Boolean = neighbours.contains(vertex)

}

class VertexNeighbourhood extends Vertex with VertexNeighbours[VertexNeighbourhood] {}


class LabelledVertexNeighbourhood(val label: Long) extends Vertex with VertexNeighbours[LabelledVertexNeighbourhood] {

  override def toString = s"$label: $neighboursString"
  def sortedNeighbours: Seq[LabelledVertexNeighbourhood] = neighbours.toSeq.sortWith(_.label < _.label)
  def neighboursString: String = {
    val first = sortedNeighbours.head.label.toString
    sortedNeighbours.drop(1).map(_.label.toString).foldLeft(first)((a,b) => s"$a, $b")
  }

}