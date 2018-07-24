package com.example

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import akka.actor.{ ActorRef, ActorSystem }
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer

/**
  * As we need something quick, we just leave it as an object,
  * but in more real-life app we would use some kind of DI, e.g. constructor parameters
  * to fully control process of initialisation and rather stay away from cake pattern
  * with its quite annoying clash of trait methods namespace
  */
object QuickstartServer extends App with UserRoutes {

  implicit val system: ActorSystem = ActorSystem("WatchlistServer")
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val watchlistActor: ActorRef = system.actorOf(WatchlistActor.props, "watchlistActor")

  lazy val routes: Route = userRoutes

  Http().bindAndHandle(routes, "localhost", 8080)

  println(s"Server online at http://localhost:8080/")

  Await.result(system.whenTerminated, Duration.Inf)
}
