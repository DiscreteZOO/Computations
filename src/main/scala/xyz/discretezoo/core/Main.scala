package xyz.discretezoo.core

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult
import akka.stream.ActorMaterializer
import io.duality.Database
import io.duality.TransactionManager.atomic
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

    def filteredGraphs(parameters: String): String = {
      val properties = parameters.split(",")
      println(parameters)
      atomic {
        val s = graphs.persistableSet.filter(graph => properties.forall(property => {
          if (property.contains("!")) {
            val pv = graph.getPropertyValueByName(property.drop(1))
            if (pv.isEmpty) false
            else pv.get.value == false
          }
          else if (property.contains(":")) {
            println("found numeric condition")
            graph.satisfiesCondition(Graph.getPropertyByName(property.split(":").head), property.split(":").drop(1).head)
          }
          else {
            val pv = graph.getPropertyValueByName(property)
            if (pv.isEmpty) false
            else pv.get.value == true
          }
        }))
        println(s.size)
      }
      "testing"
    }

    implicit val system = ActorSystem(name = "System")
    implicit val materializer = ActorMaterializer()

    val route = get {
      (path("graphs") & get) {
        parameter("par") {
          par => complete("yo") //filteredGraphs(par)
        }
      }
    }

    Http().bindAndHandle(RouteResult.route2HandlerFlow(route), "localhost", 8080)

  }

}