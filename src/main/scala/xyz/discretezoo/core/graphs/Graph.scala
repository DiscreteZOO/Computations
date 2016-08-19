package xyz.discretezoo.core.graphs

import java.sql.ResultSet
import io.duality.PersistableSet
import xyz.discretezoo.core.{PropertyValue, Property, ZooObject, ZooObjectStructure}
import xyz.discretezoo.core.externalformats.{String6, InitFileParser}
import xyz.discretezoo.snauty.Binding

/**
 * Created by katja on 10/11/15.
 */

class Graph(val string6: ValidString6, val uniqueId: String) extends ZooObject {

  val properties = new PersistableSet[PropertyValue[_]]
  val order = adjacencies.size
  
  def description = s"An undirected graph of order $order."
  def adjacencies = new String6(string6.string).parse
  def minDegree = adjacencies.mapValues(_.degree).values.min
  def maxDegree = adjacencies.mapValues(_.degree).values.max

  def getPropertyValueByName(propertyName: String): Option[PropertyValue[_]] = {
    val subset = properties.filter(p => p.property.name == propertyName)
    subset.headOption
  }

  def satisfiesCondition(property: Property[_], condition: String): Boolean = {
    val operator = """^(=|==|<=|>=|<|>)(\d+\.?\d*)$""".r
    val interval = """^([\[\(])(\d+\.?\d*),?(\d+\.?\d*)([\]\)])$""".r
    properties.exists(p => p.property == property && {
      val value = p.value.asInstanceOf[Int]
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
        case _ => condition.split(",").map(s => s.toInt == value).exists(v => v == true)
      }
    })
  }

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
  val properties = InitFileParser.getProperties(name)

  def loadFromInit(): Unit = {
    val initJSON = new InitFileParser("init.json").getJSON
    println(initJSON.apply("Graph").apply("properties").apply(0).apply("list"))
  }

  def constructFromSQLite(resultSet: ResultSet): Graph = {
    println(resultSet.getInt("zooid"))
    new Graph(new ValidString6(resultSet.getString("data")), resultSet.getString("unique_id"))
  }

}