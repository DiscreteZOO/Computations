package xyz.discretezoo.core

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult
import akka.stream.ActorMaterializer
import io.duality.Database
import xyz.discretezoo.core.externalformats.PostgresTable
import xyz.discretezoo.core.graphs.Graph

/**
 * Created by katja on 20/08/15.
 */

object Main {

  val graphs = new ZooCollection(Graph)

  def main (args: Array[String]) {

    val jdbcConnectionString = "jdbc:pgsql://localhost:5432/discretezoo?user=discretezoo&password=D!screteZ00"
    val db = new Database(jdbcConnectionString)

    db.connectRoot(this)

    graphs.updateFromSQLite()

//    val table = new PostgresTable(jdbcConnectionString)
//    val csv = table.countPerOrder()
//    println(csv)

//    implicit val system = ActorSystem(name = "System")
//    implicit val materializer = ActorMaterializer()
//
//    val route = get {
//      (path("graphs") & get) {
//        parameter("par") {
//          val pgsql = new PostgresTable(jdbcConnectionString)
//          par => complete(pgsql.filter(par))
//        }
//      } ~
//        (path("count") & get) {
//          parameter("par") {
//            val pgsql = new PostgresTable(jdbcConnectionString)
//            par => complete(pgsql.count(par))
//          }
//        } ~
//        (path("download") & get) {
//          parameter("par") {
//            val pgsql = new PostgresTable(jdbcConnectionString)
//            par => complete(pgsql.download(par, "string6"))
//          }
//        } ~
//        (path("downloadPackage") & get) {
//          parameter("par") {
//            val pgsql = new PostgresTable(jdbcConnectionString)
//            par => complete(pgsql.download(par, "package"))
//          }
//        }
//    }
//
//    Http().bindAndHandle(RouteResult.route2HandlerFlow(route), "localhost", 8080)

  }

}