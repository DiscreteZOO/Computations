package xyz.discretezoo.core

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult
import akka.stream.ActorMaterializer
import io.duality.Database
import xyz.discretezoo.core.externalformats.{SQLiteTable, PostgresTable}
import xyz.discretezoo.core.graphs.Graph

/**
 * Created by katja on 20/08/15.
 */

object Main {

  def main (args: Array[String]) {

    val jdbcConnectionString = "jdbc:pgsql://localhost:5432/discretezoo?user=discretezoo&password=D!screteZ00"
    val db = new Database(jdbcConnectionString)

    implicit val system = ActorSystem(name = "System")
    implicit val materializer = ActorMaterializer()

    val route = get {
      (path("graphs") & get) {
        parameter("par") {
          val pgsql = new PostgresTable(jdbcConnectionString)
          par => complete(pgsql.filter(par))
        }
      } ~
        (path("count") & get) {
          parameter("par") {
            val pgsql = new PostgresTable(jdbcConnectionString)
            par => complete(pgsql.count(par))
          }
        } ~
        (path("download") & get) {
          parameter("par") {
            val pgsql = new PostgresTable(jdbcConnectionString)
            par => {
              if (pgsql.count(par).toInt > 100) complete("For now it is only possible to download 100 graphs.")
              else complete(pgsql.download(par, "string6"))
            }
          }
        } ~
        (path("downloadPackage") & get) {
          parameter("par") {
            val pgsql = new PostgresTable(jdbcConnectionString)
            par => {
              if (pgsql.count(par).toInt > 100) complete("For now it is only possible to download 100 graphs.")
              else complete(pgsql.download(par, "package"))
            }
          }
        } ~
        (path("downloadSage") & get) {
          parameter("par") {
            val pgsql = new PostgresTable(jdbcConnectionString)
            par => {
              if (pgsql.count(par).toInt > 100) complete("For now it is only possible to download 100 graphs.")
              else complete(pgsql.download(par, "string6sage"))
            }
          }
        }
    }

    Http().bindAndHandle(RouteResult.route2HandlerFlow(route), "localhost", 8888)

  }

}