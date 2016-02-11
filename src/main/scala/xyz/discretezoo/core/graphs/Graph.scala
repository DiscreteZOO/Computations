package xyz.discretezoo.core.graphs

import java.sql.ResultSet
import io.duality.PersistableSet
import xyz.discretezoo.core._
import xyz.discretezoo.core.externalformats.{String6, InitFileParser}
import xyz.discretezoo.snauty.Binding

/**
 * Created by katja on 10/11/15.
 */

class Graph(val string6: ValidString6, val uniqueId: String, booleanProperties: Seq[Option[Boolean]], intProperties: Seq[Option[Int]]) extends ZooObject {

  val order = adjacencies.size
  val degree = minDegree

  val isArcTransitive = booleanProperties(0).get
  val isBipartite = booleanProperties(1).get
  val isCayley = booleanProperties(2).get
  val isDistanceRegular = booleanProperties(3).get
  val isDistanceTransitive = booleanProperties(4).get
  val isEdgeTransitive = booleanProperties(5).get
  val isHamiltonian = booleanProperties(6).get
  val isMoebiusLadder = booleanProperties(7)
  val isOverfull = booleanProperties(8).get
  val isPartialCube = booleanProperties(9).get
  val isPrism = booleanProperties(10)
  val isSplit = booleanProperties(11)
  val isStronglyRegular = booleanProperties(12).get
  val isSpx = booleanProperties(13)

  val cliqueNumber = intProperties(0).get
  val diameter = intProperties(1)
  val girth = intProperties(2)
  val oddGirth = intProperties(3)
  val symcubicIndex = intProperties(4)
  val trianglesCount = intProperties(5).get
  val truncation = intProperties(6)

  val catalogues = new PersistableSet[CatalogId]
  val aliases = new PersistableSet[String] // object alias, graph name, spx

  def description = s"An undirected graph of order $order."
  def adjacencies = new String6(string6.string).parse
  def minDegree = if (!adjacencies.isEmpty) adjacencies.mapValues(_.degree).values.min else 0
  def maxDegree = adjacencies.mapValues(_.degree).values.max

  def getBooleanProperty(sqliteName: String): Option[Boolean] = {
    sqliteName match {
      case "is_arc_transitive" => Some(isArcTransitive)
      case "is_bipartite" => Some(isBipartite)
      case "is_cayley" => Some(isCayley)
      case "is_distance_regular" => Some(isDistanceRegular)
      case "is_distance_transitive" => Some(isDistanceTransitive)
      case "is_edge_transitive" => Some(isEdgeTransitive)
      case "is_hamiltonian" => Some(isHamiltonian)
      case "is_moebius_ladder" => isMoebiusLadder
      case "is_overfull" => Some(isOverfull)
      case "is_partial_cube" => Some(isPartialCube)
      case "is_prism" => isPrism
      case "is_split" => isSplit
      case "is_strongly_regular" => Some(isStronglyRegular)
      case "is_spx" => isSpx
    }
  }

  def getIntegerProperty(sqliteName: String): Option[Int] = {
    sqliteName match {
      case "clique_number" => Some(cliqueNumber)
      case "diameter" => diameter
      case "girth" => girth
      case "odd_girth" => oddGirth
      case "symcubic_index" => symcubicIndex
      case "triangles_count" => Some(trianglesCount)
      case "truncation" => truncation
    }
  }

  def hasProperty(propertyName: String, condition: Boolean): Boolean = {
    getBooleanProperty(propertyName).map(value => value == condition).getOrElse(false)
  }

  def satisfiesCondition(propertyName: String, condition: String): Boolean = {

    val operator = """^(=|==|<=|>=|<|>)(\d+\.?\d*)$""".r
    val interval = """^([\[\(])(\d+\.?\d*),?(\d+\.?\d*)([\]\)])$""".r

    def testValue(value: Int): Boolean = {
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

    getIntegerProperty(propertyName).map(testValue(_)).getOrElse(false)

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

    def getBooleanOption(sqliteName: String): Option[Boolean] = {
      val booleanValue = resultSet.getBoolean(sqliteName)
      if (!resultSet.wasNull()) Some(booleanValue) else None
    }

    def getIntegerOption(sqliteName: String): Option[Int] = {
      val integerValue = resultSet.getInt(sqliteName)
      if (!resultSet.wasNull()) Some(integerValue) else None
    }

    val booleans = Seq(
      "is_arc_transitive",
      "is_bipartite",
      "is_cayley",
      "is_distance_regular",
      "is_distance_transitive",
      "is_edge_transitive",
      "is_hamiltonian",
      "is_moebius_ladder",
      "is_overfull",
      "is_partial_cube",
      "is_prism",
      "is_split",
      "is_strongly_regular",
      "is_spx").map(getBooleanOption(_))

    val ints = Seq(
      "clique_number",
      "diameter",
      "girth",
      "odd_girth",
      "symcubic_index",
      "triangles_count",
      "truncation").map(getIntegerOption(_))

    val graph = new Graph(new ValidString6(resultSet.getString("data")), resultSet.getString("unique_id"), booleans, ints)
    graph.catalogues += new CatalogId("cvt", resultSet.getInt("cvt_index"))
    val name = resultSet.getString("name")
    if (!resultSet.wasNull()) graph.aliases += name
    val spxr = resultSet.getString("spx_r")
    if (!resultSet.wasNull()) graph.aliases += s"SPX($spxr, ${resultSet.getString("spx_s")})"
    graph
  }

}
