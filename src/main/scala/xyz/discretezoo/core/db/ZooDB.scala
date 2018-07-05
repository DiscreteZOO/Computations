package xyz.discretezoo.core.db

import java.util.UUID

import com.sun.net.httpserver.Authenticator.Success
import xyz.discretezoo.core.db.ZooPostgresProfile.api._
import xyz.discretezoo.core.db.v1.Graphs
import xyz.discretezoo.core.maniplexes.M2orbit.M2orbitManiplex
import xyz.discretezoo.core.maniplexes.ManiplexData

import scala.concurrent.duration.{Duration, DurationLong}
import scala.concurrent.{Await, Future}
import scala.concurrent._
import ExecutionContext.Implicits.global

object ZooDB {

  private val graphs: TableQuery[Graphs] = TableQuery[Graphs]
  private val maniplexes: TableQuery[Maniplexes] = TableQuery[Maniplexes]

  private def connect: ZooPostgresProfile.backend.DatabaseDef = Database.forURL(
    "jdbc:postgresql://localhost:5432/discretezoo2",
    "discretezoo",
    "D!screteZ00",
    null,
    "org.postgresql.Driver")

  def createTables(): Unit = {
    val db = connect
    try {
      val schema = TableQuery[Maniplexes].schema
      val setup = DBIO.seq(schema.create)
      val setupFuture = db.run(setup)
      Await.result(setupFuture, 5L.seconds)
    } finally db.close()
  }

  def insertManiplexes(input: Seq[Maniplex]): Unit = {
    val db = connect
    try {
      val maniplexes = TableQuery[Maniplexes]
      val setup = DBIO.seq(maniplexes ++= input)
      val setupFuture = db.run(setup)
      Await.result(setupFuture, 5L.seconds)
    } finally db.close()
  }

  def getManiplexes: Seq[(UUID, ManiplexData, List[List[Int]])] = {
    val db = connect
    val q = for (m <- maniplexes) yield (m.uuid, m.rank, m.symmetryType, m.smallGroupOrder, m.generators)
    val f: Future[Seq[(UUID, Int, String, Int, List[List[Int]])]] = db.run(q.result)
    Await.result(f, Duration("Inf")).map(m => {
      (m._1, ManiplexData(m._2, M2orbitManiplex.deserialiseSymmetryType(m._3), m._4), m._5)
    })
  }

  def getGraphs: Seq[(Int, Int, Boolean)] = {
    val db = connect
    val q = for (g <- graphs.filter(g => g.order < 10)) yield (g.zooid, g.order, g.isArcTransitive)
    val f: Future[Seq[(Int, Int, Boolean)]] = db.run(q.result)
    Await.result(f, Duration("Inf"))
  }

}