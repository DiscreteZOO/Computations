package xyz.discretezoo.core.primitives

sealed trait Element {}

class Vertex extends Element

class Edge(vertex1: Vertex, vertex2: Vertex) extends Element {
  val endVertices = Set(vertex1, vertex2)
  def isAdjacent(edge: Edge): Boolean = endVertices.intersect(edge.endVertices).size == 1
}