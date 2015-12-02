import scala.collection.mutable
import scala.util.matching.Regex

/**
  * Created by katja on 17/11/15.
  */
object Util {

  def commaJoin(a: Any, b: Any): String = s"$a, $b"

  def buildAdjacencyList(adjacencies: Any): Map[Int, LabelledVertexNeighbourhood] = {

    val verticesByIds = mutable.Map[Int, LabelledVertexNeighbourhood]()

    adjacencies match {
      case edgeListString: String => {
        val edgePattern = new Regex("""(\d+)[\ ,]+(\d+)""", "v1", "v2")
        edgePattern.findAllIn(edgeListString).matchData.foreach(edge => {
          addEdge(edge.group("v1").toInt, edge.group("v2").toInt)
        })
      }
      case indexMap: Map[Int, Set[Int]] => {
        indexMap.foreach(pair => pair._2.foreach(vertex => addEdge(pair._1, vertex)))
      }
    }

    def addEdge(vertexNo1: Int, vertexNo2: Int): Unit = {
      val vertex1 = verticesByIds.getOrElseUpdate(vertexNo1, new LabelledVertexNeighbourhood(vertexNo1))
      val vertex2 = verticesByIds.getOrElseUpdate(vertexNo2, new LabelledVertexNeighbourhood(vertexNo2))
      vertex1.addNeighbour(vertex2)
      vertex2.addNeighbour(vertex1)
    }

    verticesByIds.values.zipWithIndex.map(_.swap).toMap

  }

}
