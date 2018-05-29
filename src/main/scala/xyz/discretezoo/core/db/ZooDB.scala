package xyz.discretezoo.core.db

//import slick.jdbc.PostgresProfile.api._
import xyz.discretezoo.core.db.ZooPostgresProfile.api._
import xyz.discretezoo.core.db.Maniplex

import scala.concurrent.duration.DurationLong
import scala.concurrent.{Await, Future}

object ZooDB {

  private def connect = Database.forURL(
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
    } finally db.close
  }

  def insertManiplexes(input: Seq[Maniplex]): Unit = {
    val db = connect
    try {
      val maniplexes = TableQuery[Maniplexes]
      val setup = DBIO.seq(maniplexes ++= input)
      val setupFuture = db.run(setup)
      Await.result(setupFuture, 5L.seconds)
    } finally db.close
  }

}
