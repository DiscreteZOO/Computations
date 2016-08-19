package xyz.discretezoo.core.graphs

import scala.collection.mutable

/**
  * Created by katja on 23/11/15.
  */
class ArcN(val edges: Seq[Edge]) {

  require(ArcN.isArc(edges), "The input sequence is not a proper path.")

  val length = edges.size
  val vertices = ArcN.vertexAppearanceSequence(edges).map(_._1)

  def reverse: ArcN = new ArcN(edges.reverse)

  def isShunt(arc: ArcN): Boolean = {
    length == arc.length && vertices.endsWith(arc.vertices.init) && (length == 1 || edges.endsWith(arc.edges.init) || arc.edges.endsWith(edges.init))
  }

}

object ArcN {

  private def vertexAppearanceSequence(edges: Seq[Edge]): Seq[(Vertex, Int)] = {

    val vertexEdges = new mutable.HashMap[Vertex, mutable.Set[Int]] with mutable.MultiMap[Vertex, Int]

    edges.zipWithIndex.foreach(pair => pair._1.endVertices.foreach(vertex => vertexEdges.addBinding(vertex, pair._2)))

    def lt(v1: (Vertex, mutable.Set[Int]), v2: (Vertex, mutable.Set[Int])): Boolean = {
      v1._2.size < v2._2.size || v1._2.size == v2._2.size &&  v1._2.min < v2._2.min
    }

    vertexEdges.toSeq.sortWith(lt).map(pair => (pair._1, pair._2.size))
  }

  def vertexSequence(edges: Seq[Edge]): Seq[Vertex] = vertexAppearanceSequence(edges).map(pair => pair._1)

  def endVertices(edges: Seq[Edge]): Seq[Vertex] = vertexAppearanceSequence(edges).filter(_._2 == 1).map(_._1)

  def interior(edges: Seq[Edge]): Seq[Vertex] = vertexAppearanceSequence(edges).filter(_._2 > 1).map(_._1)

  def interiorTest(edges: Seq[Edge]): Boolean = {
    edges.length <= 1 || {
      val slider = edges.sliding(2, 1).map(pair => pair.head.endVertices.intersect(pair.last.endVertices))
      val intersectionSizes = vertexAppearanceSequence(edges).filter(_._2 == 2).size == edges.size-1
      endVertices(edges).size == 1 && intersectionSizes && slider.forall(_.size == 1)
    }
  }

  def isArc(edges: Seq[Edge]): Boolean = {
    interiorTest(edges) && endVertices(edges).size == 2
  }

}