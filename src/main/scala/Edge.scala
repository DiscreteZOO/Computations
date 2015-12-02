/**
  * Created by katja on 22/11/15.
  */
class Edge(vertex1: Vertex, vertex2: Vertex) extends Element {

  val endVertices = Set(vertex1, vertex2)

  def isAdjacent(edge: Edge): Boolean = endVertices.intersect(edge.endVertices).size == 1

}
