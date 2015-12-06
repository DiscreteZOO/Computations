

/**
 * Created by katja on 10/11/15.
 */

class Graph(val adjacencies: Map[Int, LabelledVertexNeighbourhood]) {

  val order = adjacencies.size
  val dynamicFields = new GraphDynamicProperties(Map("is_cayley" -> false))

  def description = s"An undirected graph of order $order."
  def minDegree = adjacencies.mapValues(_.degree).values.min
  def maxDegree = adjacencies.mapValues(_.degree).values.max

  def neighbourSets: Map[Int, Set[Int]] = adjacencies.map(m => m._1 -> m._2.neighbours.map(_.label).toSet)
  def nautyCanonical: String = new Binding().callSparseNauty(neighbourSets)

  def getSparse6(labeller: String): String = {
    labeller match {
      case "Nauty" => new Binding().callSparseNauty(neighbourSets)
    }
  }

  def properties: Unit = { dynamicFields.fields.foreach(println) }

  override def toString() = {
    val orderedAdjacencies = adjacencies.values.toSeq.sortWith(_.label < _.label)
    orderedAdjacencies.foldLeft(s"$description\n")((a, b) => s"$a${b.toString}\n")
  }
  
  // fields

  val connected_components_number: Option[Int] = None
//  val data: String = ??? // unique
  val has_multiple_edges: Option[Boolean] = None
//  val name: String = ??? // unique
  val number_of_loops: Option[Int] = None
  val odd_girth: Option[Int] = None
  val radius: Option[Int] = None
  val size: Option[Int] = None // number of edges
  val spanning_trees_count: Option[Int] = None
  val szeged_index: Option[Int] = None
  val triangles_count: Option[Int] = None
  val treewidth: Option[Int] = None
//  val unique_id: String = ??? // unique
  val vertex_connectivity: Option[Int] = None
  val wiener_index: Option[Int] = None
  val zagreb1_index: Option[Int] = None
  val zagreb2_index: Option[Int] = None

}
