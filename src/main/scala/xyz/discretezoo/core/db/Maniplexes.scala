package xyz.discretezoo.core.db

import java.util.UUID

import xyz.discretezoo.core.db.ZooPostgresProfile.api._

case class Maniplex(
  uuid: UUID,
  rank: Int,
  symmetryType: String,
  smallGroupOrder: Int,
  smallGroupId: Int,
  generators: List[List[Int]],
  flagGraph: String,
  underlyingGraph: String,
  orbits: Int)

class Maniplexes(tag: Tag) extends Table[Maniplex](tag, "maniplexes") {

  def uuid = column[UUID]("UUID", O.PrimaryKey)
  def rank = column[Int]("RANK")
  def symmetryType = column[String]("SYMMETRY_TYPE") // the set I for 1-orbit maniplexes
  def smallGroupOrder = column[Int]("SMALL_GROUP_ORDER")
  def smallGroupId = column[Int]("SMALL_GROUP_ID")
  def generators = column[List[List[Int]]]("GENERATORS")
  def flagGraph = column[String]("FLAG_GRAPH")
  def underlyingGraph = column[String]("UNDERLYING_GRAPH")
  def orbits = column[Int]("ORBITS")

  def * = (
    uuid,
    rank,
    symmetryType,
    smallGroupOrder,
    smallGroupId,
    generators,
    flagGraph,
    underlyingGraph,
    orbits
  ) <> (Maniplex.tupled, Maniplex.unapply)
}
