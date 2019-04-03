package xyz.discretezoo.core.db

import java.util.UUID

import xyz.discretezoo.core.db.ZooPostgresProfile.api._

case class Maniplex(
  uuid: UUID,
  flagGraph: String,
  generators: List[List[Int]],
  orbits: Int,
  rank: Int,
  isPolytope: Boolean,
  isRegular: Boolean,
  smallGroupId: Int,
  smallGroupOrder: Int,
  symmetryType: String,
  underlyingGraph: String
)

class Maniplexes(tag: Tag) extends Table[Maniplex](tag, "maniplexes") {

  def uuid = column[UUID]("UUID", O.PrimaryKey)
  def flagGraph = column[String]("FLAG_GRAPH")
  def generators = column[List[List[Int]]]("GENERATORS")
  def orbits = column[Int]("ORBITS")
  def rank = column[Int]("RANK")
  def isPolytope = column[Boolean]("IS_POLYTOPE")
  def isRegular = column[Boolean]("IS_REGULAR")
  def smallGroupId = column[Int]("SMALL_GROUP_ID")
  def smallGroupOrder = column[Int]("SMALL_GROUP_ORDER")
  def symmetryType = column[String]("SYMMETRY_TYPE") // the set I for 1-orbit maniplexes
  def underlyingGraph = column[String]("UNDERLYING_GRAPH")

  def * = (
    uuid,
    flagGraph,
    generators,
    orbits,
    rank,
    isPolytope,
    isRegular,
    smallGroupId,
    smallGroupOrder,
    symmetryType,
    underlyingGraph,
  ) <> (Maniplex.tupled, Maniplex.unapply)
}
