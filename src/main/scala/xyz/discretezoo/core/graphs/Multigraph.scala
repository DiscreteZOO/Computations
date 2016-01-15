package xyz.discretezoo.core.graphs

/**
  * Created by katja on 22/11/15.
  */
class Multigraph(edgeList: Map[Tuple2[Int, Int], Int]) {

  // TODO Tuple2 is ordered; edges can appear twice

  val verticesByIds = buildVerticesByIds
  val edges: Set[Edge] = buildEdges
  val vertices: Set[Vertex] = verticesByIds.values.toSet
  val order = vertices.size
  val description = s"Multigraph of order $order."

  def buildVerticesByIds: Map[Int, Vertex] = {
    edgeList.flatMap(pair => Set(pair._1._1, pair._1._2)).toSet[Int].map(index => (index, new Vertex)).toMap
  }

  def buildEdges: Set[Edge] = {

    def edgeSetFromInt(endvertices: Tuple2[Int, Int], parallelEdges: Int): Set[Edge] = {
      val vertex1 = verticesByIds.get(endvertices._1).get
      val vertex2 = verticesByIds.get(endvertices._2).get
      (0 until parallelEdges).map(index => index -> new Edge(vertex1, vertex2)).toMap.values.toSet
    }

    edgeList.map(pair => edgeSetFromInt(pair._1, pair._2)).toSet.flatten[Edge]

  }

  override def toString() = {
    edgeList.map(pair => s"{${pair._1._1}, ${pair._1._2}}: ${pair._2}").foldLeft(s"$description\n")((a, b) => s"$a$b\n")
  }

}
