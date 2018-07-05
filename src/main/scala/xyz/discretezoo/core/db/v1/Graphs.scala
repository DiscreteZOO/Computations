package xyz.discretezoo.core.db.v1

import java.util.UUID

import xyz.discretezoo.core.db.ZooPostgresProfile.api._

case class GraphTypes(
  isBipartite: Boolean,
  isCartesianProduct: Boolean,
  isChordal: Boolean,
  isCirculant: Boolean,
  isCircularPlanar: Boolean,
  isDistanceRegular: Boolean,
  isDistanceTransitive: Boolean,
  isEulerian: Boolean,
  isForest: Boolean,
  isGallaiTree: Boolean,
  isHamiltonian: Boolean,
  isInterval: Boolean,
  isLineGraph: Boolean,
  isPartialCube: Boolean,
  isPerfect: Boolean,
  isPlanar: Boolean,
  isPrime: Boolean,
  isRegular: Boolean,
  isSplit: Boolean,
  isStronglyRegular: Boolean,
  isTree: Boolean
)

case class OtherProperties(
  averageDistance: Float,
  chromaticIndex: Int,
  chromaticNumber: Int,
  clusterTransitivity: Float,
  clusteringAverage: Float,
  fractionalChromaticIndex: Int,
  isAsteroidalTripleFree: Boolean,
  isEvenHoleFree: Boolean,
  isLongAntiholeFree: Boolean,
  isLongHoleFree: Boolean,
  isOddHoleFree: Boolean,
  isOverfull: Boolean,
  lovaszTheta: Float,
  maximumAverageDegree: Float,
  spanningTreesCount: Int,
  szegedIndex: Int,
  treewidth: Int,
  wienerIndex: Int,
  zagreb1Index: Int,
  zagreb2Index: Int
)

case class SymmetryProperties(
  isArcTransitive: Boolean,
  isCayley: Boolean,
  isEdgeTransitive: Boolean,
  isVertexTransitive: Boolean
)

case class Graph(
  zooid: Int,
  data: String,
  name: String,
  order: Int,
  averageDegree: Float,
  cliqueNumber: Int,
  connectedComponentsNumber: Int,
  diameter: Int,
  edgeConnectivity: Int,
  genus: Int,
  girth: Int,
  hasMultipleEdges: Boolean,
  numberOfLoops: Int,
  oddGirth: Int,
  radius: Int,
  size: Int,
  trianglesCount: Int,
  vertexConnectivity: Int,
  graphTypes: GraphTypes,
  symmetryProperties: SymmetryProperties,
  otherProperties: OtherProperties
)

class Graphs(tag: Tag) extends Table[Graph](tag, "graph") {

  def zooid = column[Int]("zooid", O.PrimaryKey)
  def data = column[String]("data")

  def averageDegree = column[Float]("average_degree")
  def averageDistance = column[Float]("average_distance")
  def name = column[String]("name")
  def order = column[Int]("order")
  def chromaticIndex = column[Int]("chromatic_index")
  def chromaticNumber = column[Int]("chromatic_number")
  def cliqueNumber = column[Int]("clique_number")
  def clusterTransitivity = column[Float]("cluster_transitivity")
  def clusteringAverage = column[Float]("clustering_average")
  def connectedComponentsNumber = column[Int]("connected_components_number")
  def diameter = column[Int]("diameter")
  def edgeConnectivity = column[Int]("edge_connectivity")
  def fractionalChromaticIndex = column[Int]("fractional_chromatic_index")
  def genus = column[Int]("genus")
  def girth = column[Int]("girth")
  def hasMultipleEdges = column[Boolean]("has_multiple_edges")
  def isArcTransitive = column[Boolean]("is_arc_transitive")
  def isAsteroidalTripleFree = column[Boolean]("is_asteroidal_triple_free")
  def isBipartite = column[Boolean]("is_bipartite")
  def isCartesianProduct = column[Boolean]("is_cartesian_product")
  def isCayley = column[Boolean]("is_cayley")
  def isChordal = column[Boolean]("is_chordal")
  def isCirculant = column[Boolean]("is_circulant")
  def isCircularPlanar = column[Boolean]("is_circular_planar")
  def isDistanceRegular = column[Boolean]("is_distance_regular")
  def isDistanceTransitive = column[Boolean]("is_distance_transitive")
  def isEdgeTransitive = column[Boolean]("is_edge_transitive")
  def isEulerian = column[Boolean]("is_eulerian")
  def isEvenHoleFree = column[Boolean]("is_even_hole_free")
  def isForest = column[Boolean]("is_forest")
  def isGallaiTree = column[Boolean]("is_gallai_tree")
  def isHamiltonian = column[Boolean]("is_hamiltonian")
  def isInterval = column[Boolean]("is_interval")
  def isLineGraph = column[Boolean]("is_line_graph")
  def isLongAntiholeFree = column[Boolean]("is_long_antihole_free")
  def isLongHoleFree = column[Boolean]("is_long_hole_free")
  def isOddHoleFree = column[Boolean]("is_odd_hole_free")
  def isOverfull = column[Boolean]("is_overfull")
  def isPartialCube = column[Boolean]("is_partial_cube")
  def isPerfect = column[Boolean]("is_perfect")
  def isPlanar = column[Boolean]("is_planar")
  def isPrime = column[Boolean]("is_prime")
  def isRegular = column[Boolean]("is_regular")
  def isSplit = column[Boolean]("is_split")
  def isStronglyRegular = column[Boolean]("is_strongly_regular")
  def isTree = column[Boolean]("is_tree")
  def isVertexTransitive = column[Boolean]("is_vertex_transitive")
  def lovaszTheta = column[Float]("lovasz_theta")
  def maximumAverageDegree = column[Float]("maximum_average_degree")
  def numberOfLoops = column[Int]("number_of_loops")
  def oddGirth = column[Int]("odd_girth")
  def radius = column[Int]("radius")
  def size = column[Int]("size")
  def spanningTreesCount = column[Int]("spanning_trees_count")
  def szegedIndex = column[Int]("szeged_index")
  def treewidth = column[Int]("treewidth")
  def trianglesCount = column[Int]("triangles_count")
  def vertexConnectivity = column[Int]("vertex_connectivity")
  def wienerIndex = column[Int]("wiener_index")
  def zagreb1Index = column[Int]("zagreb1_index")
  def zagreb2Index = column[Int]("zagreb1_index")

  def graphTypesProjection = (
    isBipartite,
    isCartesianProduct,
    isChordal,
    isCirculant,
    isCircularPlanar,
    isDistanceRegular,
    isDistanceTransitive,
    isEulerian,
    isForest,
    isGallaiTree,
    isHamiltonian,
    isInterval,
    isLineGraph,
    isPartialCube,
    isPerfect,
    isPlanar,
    isPrime,
    isRegular,
    isSplit,
    isStronglyRegular,
    isTree
  ) <> ((GraphTypes.apply _).tupled, GraphTypes.unapply)

  def otherPropertiesProjection = (
    averageDistance,
    chromaticIndex,
    chromaticNumber,
    clusterTransitivity,
    clusteringAverage,
    fractionalChromaticIndex,
    isAsteroidalTripleFree,
    isEvenHoleFree,
    isLongAntiholeFree,
    isLongHoleFree,
    isOddHoleFree,
    isOverfull,
    lovaszTheta,
    maximumAverageDegree,
    spanningTreesCount,
    szegedIndex,
    treewidth,
    wienerIndex,
    zagreb1Index,
    zagreb2Index
  ) <> ((OtherProperties.apply _).tupled, OtherProperties.unapply)

  def symmetryPropertiesProjection = (
    isArcTransitive,
    isCayley,
    isEdgeTransitive,
    isVertexTransitive
  ) <> ((SymmetryProperties.apply _).tupled, SymmetryProperties.unapply)

  def * = (
    zooid,
    data,
    name,
    order,
    averageDegree,
    cliqueNumber,
    connectedComponentsNumber,
    diameter,
    edgeConnectivity,
    genus,
    girth,
    hasMultipleEdges,
    numberOfLoops,
    oddGirth,
    radius,
    size,
    trianglesCount,
    vertexConnectivity,
    graphTypesProjection,
    symmetryPropertiesProjection,
    otherPropertiesProjection
  ) <> ((Graph.apply _).tupled, Graph.unapply)

}