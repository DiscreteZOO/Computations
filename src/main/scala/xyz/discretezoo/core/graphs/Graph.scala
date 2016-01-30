package xyz.discretezoo.core.graphs

import java.sql.ResultSet
import io.duality.PersistableSet
import xyz.discretezoo.core.{PropertyValue, Property, ZooObject, ZooObjectStructure}
import xyz.discretezoo.core.externalformats.{String6, InitFileParser}
import xyz.discretezoo.snauty.Binding

/**
 * Created by katja on 10/11/15.
 */

class Graph(val string6: ValidString6, val uniqueId: String, booleanProperties: Seq[Boolean], intProperties: Seq[Int]) extends ZooObject {

  val order = adjacencies.size

  val isArcTransitive = booleanProperties(0)
  val isBipartite = booleanProperties(1)
  val isCayley = booleanProperties(2)
  val isDistanceRegular = booleanProperties(3)
  val isDistanceTransitive = booleanProperties(4)
  val isEdgeTransitive = booleanProperties(5)
  val isEulerian = booleanProperties(6)
  val isMoebiusLadder = booleanProperties(7)
  val isPrism = booleanProperties(8)
  val isSplit = booleanProperties(9)
  val isStronglyRegular = booleanProperties(10)
  val isSpx = booleanProperties(11)
  val cliqueNumber = intProperties(0)
  val diameter = intProperties(1)
  val girth = intProperties(2)
  val oddGirth = intProperties(3)
  val trianglesCount = intProperties(4)

  def description = s"An undirected graph of order $order."
  def adjacencies = new String6(string6.string).parse
  def minDegree = adjacencies.mapValues(_.degree).values.min
  def maxDegree = adjacencies.mapValues(_.degree).values.max

  def hasProperty(propertyName: String, condition: Boolean): Boolean = {
    propertyName match {
      case "is_arc_transitive" => isArcTransitive == condition
      case "is_bipartite" => isBipartite == condition
      case "is_cayley" => isCayley == condition
      case "is_distance_regular" => isDistanceRegular == condition
      case "is_distance_transitive" => isDistanceTransitive == condition
      case "is_edge_transitive" => isEdgeTransitive == condition
      case "is_eulerian" => isEulerian == condition
      case "is_moebius_ladder" => isMoebiusLadder == condition
      case "is_prism" => isPrism == condition
      case "is_split" => isSplit == condition
      case "is_strongly_regular" => isStronglyRegular == condition
      case "is_spx" => isSpx == condition
    }
  }

  def satisfiesCondition(propertyName: String, condition: String): Boolean = {
    val operator = """^(=|==|<=|>=|<|>)(\d+\.?\d*)$""".r
    val interval = """^([\[\(])(\d+\.?\d*),?(\d+\.?\d*)([\]\)])$""".r
    val value = propertyName match {
      case "order" => order
      case "clique_number" => cliqueNumber
      case "diameter" => diameter
      case "girth" => girth
      case "odd_girth" => oddGirth
      case "triangles_count" => trianglesCount
    }
    condition match {
      case operator(op, n) => op match {
        case "=" => value == n.toInt
        case "==" => value == n.toInt
        case "<=" => value <= n.toInt
        case ">=" => value >= n.toInt
        case "<" => value < n.toInt
        case ">" => value > n.toInt
      }
      case interval(leftBrace, lowerBound, upperBound, rightBrace) => {
        {leftBrace match {
          case "(" => lowerBound.toInt < value
          case "[" => lowerBound.toInt <= value
        }} &&
        {rightBrace match {
          case ")" => value < upperBound.toInt
          case "]" => value <= upperBound.toInt
        }}
      }
      case _ => condition.split(",").exists(s => s.toInt == value)
    }
  }

  def neighbourSets: Map[Long, Set[Long]] = adjacencies.map(m => m._1 -> m._2.neighbours.map(_.label).toSet)
  def nautyCanonical: String = new Binding().callSparseNauty(neighbourSets)

  def getSparse6(labeller: String): String = {
    labeller match {
      case "Nauty" => new Binding().callSparseNauty(neighbourSets)
    }
  }

  override def toString = {
    val orderedAdjacencies = adjacencies.values.toSeq.sortWith(_.label < _.label)
    orderedAdjacencies.foldLeft(s"$description\n")((a, b) => s"$a${b.toString}\n")
  }

}

object Graph extends ZooObjectStructure[Graph] {

  val name = "Graph" // for JSON
  val tableSQLite = "graph"
  val properties = InitFileParser.getProperties(name)

  def loadFromInit(): Unit = {
    val initJSON = new InitFileParser("init.json").getJSON
    println(initJSON.apply("Graph").apply("properties").apply(0).apply("list"))
  }

  def constructFromSQLite(resultSet: ResultSet): Graph = {
    println(resultSet.getInt("zooid"))

    val booleans = Seq("is_arc_transitive", "is_bipartite", "is_cayley", "is_distance_regular", "is_distance_transitive", "is_edge_transitive", "is_eulerian", "is_moebius_ladder", "is_prism", "is_split", "is_strongly_regular", "is_spx").map(resultSet.getBoolean(_))
    val ints = Seq("clique_number", "diameter", "girth", "odd_girth", "triangles_count").map(resultSet.getInt(_))
    new Graph(new ValidString6(resultSet.getString("data")), resultSet.getString("unique_id"), booleans, ints)
  }

}