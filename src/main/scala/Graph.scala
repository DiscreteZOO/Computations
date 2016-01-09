import java.sql.ResultSet


/**
 * Created by katja on 10/11/15.
 */

class Graph(val adjacencies: Map[Long, LabelledVertexNeighbourhood]) extends ZooObject {

  val order = adjacencies.size

  def description = s"An undirected graph of order $order."
  def minDegree = adjacencies.mapValues(_.degree).values.min
  def maxDegree = adjacencies.mapValues(_.degree).values.max

  def neighbourSets: Map[Long, Set[Long]] = adjacencies.map(m => m._1 -> m._2.neighbours.map(_.label).toSet)
  def nautyCanonical: String = new Binding().callSparseNauty(neighbourSets)

  def getSparse6(labeller: String): String = {
    labeller match {
      case "Nauty" => new Binding().callSparseNauty(neighbourSets)
    }
  }

  override def toString() = {
    val orderedAdjacencies = adjacencies.values.toSeq.sortWith(_.label < _.label)
    orderedAdjacencies.foldLeft(s"$description\n")((a, b) => s"$a${b.toString}\n")
  }

}

object Graph extends ZooObjectStructure[Graph] {

  val name = "Graph" // for JSON
  val tableSQLite = "graph"

  def constructFromSQLite(resultSet: ResultSet) = new Graph(new String6(resultSet.getString("data")).parse)

}