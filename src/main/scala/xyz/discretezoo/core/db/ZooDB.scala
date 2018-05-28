package xyz.discretezoo.core.db

//import slick.jdbc.PostgresProfile.api._
import xyz.discretezoo.core.db.ZooPostgresProfile.api._
import xyz.discretezoo.core.db.maniplexes.{Maniplex, Maniplexes}

import scala.concurrent.duration.DurationLong
import scala.concurrent.{Await, Future}

object ZooDB {

  private def connect = Database.forURL(
    "jdbc:postgresql://localhost:5432/discretezoo2",
    "discretezoo",
    "D!screteZ00",
    null,
    "org.postgresql.Driver")

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
