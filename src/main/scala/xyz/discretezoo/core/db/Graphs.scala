package xyz.discretezoo.core.db

import java.util.UUID

import xyz.discretezoo.core.db.ZooPostgresProfile.api._

case class BasicGraphProperties(
  averageDegree: Float,
  connectedComponentsNumber: Int,
  diameter: Int,
  girth: Int,
  hasMultipleEdges: Boolean,
  isBipartite: Boolean,
  isForest: Boolean,
  isRegular: Boolean,
  isTree: Boolean,
  numberOfLoops: Int,
  oddGirth: Int,
  order: Int,
  size: Int,
  trianglesCount: Int
)

case class SymmetryProperties(
  isArcTransitive: Boolean,
  isCayley: Boolean,
  isEdgeTransitive: Boolean,
  isVertexTransitive: Boolean
)

case class Graph(
  uUID: UUID,
  data: String,
  basicProperties: BasicGraphProperties,
  symmetryProperties: SymmetryProperties,
  cliqueNumber: Int,
  isDistanceRegular: Boolean,
  isDistanceTransitive: Boolean,
  isEulerian: Boolean,
  isHamiltonian: Boolean,
  isOverfull: Boolean,
  isPartialCube: Boolean,
  isSplit: Boolean,
  isStronglyRegular: Boolean
)

class Graphs(tag: Tag) extends Table[Graph](tag, "graphs") {

  def uuid = column[UUID]("UUID", O.PrimaryKey)
  def averageDegree = column[Float]("AVERAGE_DEGREE")
  def cliqueNumber = column[Int]("CLIQUE_NUMBER")
  def connectedComponentsNumber = column[Int]("CONNECTED_COMPONENTS_NUMBER")
  def data = column[String]("DATA")
  def diameter = column[Int]("DIAMETER")
  def girth = column[Int]("GIRTH")
  def hasMultipleEdges = column[Boolean]("HAS_MULTIPLE_EDGES")
  def isArcTransitive = column[Boolean]("IS_ARC_TRANSITIVE")
  def isBipartite = column[Boolean]("IS_BIPARTITE")
  def isCayley = column[Boolean]("IS_CAYLEY")
  def isDistanceRegular = column[Boolean]("IS_DISTANCE_REGULAR")
  def isDistanceTransitive = column[Boolean]("IS_DISTANCE_TRANSITIVE")
  def isEdgeTransitive = column[Boolean]("IS_EDGE_TRANSITIVE")
  def isEulerian = column[Boolean]("IS_EULERIAN")
  def isForest = column[Boolean]("IS_FOREST")
  def isHamiltonian = column[Boolean]("IS_HAMILTONIAN")
  def isOverfull = column[Boolean]("IS_OVERFULL")
  def isPartialCube = column[Boolean]("IS_PARTIAL_CUBE")
  def isRegular = column[Boolean]("IS_REGULAR")
  def isSplit = column[Boolean]("IS_SPLIT")
  def isStronglyRegular = column[Boolean]("IS_STRONGLY_REGULAR")
  def isTree = column[Boolean]("IS_TREE")
  def isVertexTransitive = column[Boolean]("IS_VERTEX_TRANSITIVE")
  def numberOfLoops = column[Int]("NUMBER_OF_LOOPS")
  def oddGirth = column[Int]("ODD_GIRTH")
  def order = column[Int]("ORDER")
  def size = column[Int]("SIZE")
  def trianglesCount = column[Int]("TRIANGLES_COUNT")

  def basicPropertiesProjection = (
    averageDegree,
    connectedComponentsNumber,
    diameter,
    girth,
    hasMultipleEdges,
    isBipartite,
    isForest,
    isRegular,
    isTree,
    numberOfLoops,
    oddGirth,
    order,
    size,
    trianglesCount
  ) <> ((BasicGraphProperties.apply _).tupled, BasicGraphProperties.unapply)

  def symmetryPropertiesProjection = (
    isArcTransitive,
    isCayley,
    isEdgeTransitive,
    isVertexTransitive
  ) <> ((SymmetryProperties.apply _).tupled, SymmetryProperties.unapply)

  def * = (
    uuid,
    data,
    basicPropertiesProjection,
    symmetryPropertiesProjection,
    cliqueNumber,
    isDistanceRegular,
    isDistanceTransitive,
    isEulerian,
    isHamiltonian,
    isOverfull,
    isPartialCube,
    isSplit,
    isStronglyRegular
  ) <> ((Graph.apply _).tupled, Graph.unapply)

}