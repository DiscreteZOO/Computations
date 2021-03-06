package xyz.discretezoo.core.graphs

/**
  * Created by katja on 17/11/15.
  */
class LabelledVertexNeighbourhood(val label: Long) extends Vertex with VertexNeighbours[LabelledVertexNeighbourhood] {

  override def toString = s"$label: $neighboursString"
  def sortedNeighbours = neighbours.toSeq.sortWith(_.label < _.label)
  def neighboursString: String = {
    val first = sortedNeighbours.head.label.toString
    sortedNeighbours.drop(1).map(_.label.toString).foldLeft(first)((a,b) => s"$a, $b")
  }

}
