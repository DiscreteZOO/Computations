package xyz.discretezoo.core.graphs

/**
  * Created by katja on 17/11/15.
  */
class RegularGraph(adjacencies: Map[Long, LabelledVertexNeighbourhood], uniqueId: String) extends Graph(adjacencies, uniqueId) {

  require(minDegree == maxDegree, "The input is not a regular graph")

  val degree = minDegree
  override val description = s"An undirected graph of order $order and degree $degree."

}
