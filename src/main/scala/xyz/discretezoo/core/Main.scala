package xyz.discretezoo.core

import java.sql.{ResultSet, Statement, DriverManager}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult
import akka.stream.ActorMaterializer
import io.duality.Database
import io.duality.TransactionManager.atomic
import xyz.discretezoo.core.externalformats.PostgresTable
import xyz.discretezoo.core.graphs.Graph

/**
 * Created by katja on 20/08/15.
 */

object Main {

  val graphs = new ZooCollection(Graph)

  def main (args: Array[String]) {

    val db = new Database("jdbc:pgsql://localhost:5432/graphzoo?user=graphzoo&password=gr4ph!Z00")

    db.connectRoot(this)
//    graphs.updateFromSQLite

    implicit val system = ActorSystem(name = "System")
    implicit val materializer = ActorMaterializer()

    val route = get {
      (path("graphs") & get) {
        parameter("par") {
          val pgsql = new PostgresTable
          par => complete(pgsql.filter("girth:=4&!isBipartite"))
        }
      }
    }

    Http().bindAndHandle(RouteResult.route2HandlerFlow(route), "localhost", 8080)

  }

}