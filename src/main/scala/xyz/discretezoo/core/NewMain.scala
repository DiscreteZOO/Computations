package xyz.discretezoo.core

import akka.actor.{TypedActor, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{UpgradeToWebsocket, Message}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RouteResult, ExpectedWebsocketRequestRejection}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Flow
import org.reactivestreams.Processor

import scala.io.StdIn

object NewMain {

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem(name = "System")
    implicit val materializer = ActorMaterializer()

    val route = get {
      pathSingleSlash {
        complete("Hello")
      } ~
        path("ping") {
          complete("PONG!")
        } ~
        path("crash") {
          sys.error("BOOM!")
        }
    }

    Http().bindAndHandle(RouteResult.route2HandlerFlow(route), "localhost", 8080)

  }

}
