package xyz.discretezoo.core

import xyz.discretezoo.core.graphs.{AdjacencyListBuilder, LabelledVertexNeighbourhood}
import scala.util.matching.Regex

/**
  * Created by katja on 17/11/15.
  */
object Util {

  def commaJoin(a: Any, b: Any): String = s"$a, $b"

  // TODO requirements on input
  def buildAdjacencyList(adjacencies: Any): Map[Long, LabelledVertexNeighbourhood] = {

    val adjacencyListBuilder = new AdjacencyListBuilder()

    adjacencies match {
      case edgeListString: String => {
        val edgePattern = new Regex("""(\d+)[\ ,]+(\d+)""", "v1", "v2")
        edgePattern.findAllIn(edgeListString).matchData.foreach(edge => {
          adjacencyListBuilder.addEdge(edge.group("v1").toInt, edge.group("v2").toInt)
        })
      }
//      case edgeListInt:
      case indexMap: Map[Long, Set[Long]] => {
        indexMap.foreach(pair => pair._2.foreach(vertex => adjacencyListBuilder.addEdge(pair._1, vertex)))
      }
    }

    adjacencyListBuilder.getMap

  }

}
