package com.example

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{PathMatcher, PathMatcher1, Route}
import akka.http.scaladsl.server.directives.MethodDirectives.{get, post}
import akka.http.scaladsl.server.directives.PathDirectives.path
import akka.http.scaladsl.server.directives.RouteDirectives.complete
import akka.pattern.ask
import akka.util.Timeout
import com.example.WatchlistActor._
import io.circe.generic.auto._

import scala.concurrent.duration._

final case class UserId(userId: Int) extends AnyVal

object UserId {
  val userIdMatcher: PathMatcher1[UserId] = {
    PathMatcher("[0-9]{5}".r).map { path =>
      UserId(path.toInt)
    }
  }
}

trait UserRoutes extends JsonSupport {

  implicit def system: ActorSystem

  lazy val log = Logging(system, classOf[UserRoutes])

  def watchlistActor: ActorRef

  implicit lazy val timeout = Timeout(5.seconds) // usually we'd obtain the timeout from the system's configuration

  lazy val userRoutes: Route =
    pathPrefix("user" / UserId.userIdMatcher / "watchlist") { userId =>
      log.debug(s"user $userId watchlist")
      concat(
        pathEndOrSingleSlash {
          concat(
            get {
              val watchlist = (watchlistActor ? ListItems(userId)).mapTo[Watchlist]

              onSuccess(watchlist) { list =>
                log.debug(s"user $userId watchlist: $watchlist")
                complete(list)
              }
            },
            post {
              log.info(s"POST user/$userId/watchlist")
              entity(as[Watchlist]) { itemsToAdd =>
                // Perhaps a nicer validator would use Validate/Either, we could do it as a separate step after reading json
                // but before creating case class. We can put it to backlog...
                validate(WatchlistHelper.validate(itemsToAdd), WatchlistHelper.validateMsg) {
                  val add = (watchlistActor ? AddItems(userId, itemsToAdd.watchlistIds)).mapTo[ActionPerformed]

                  onSuccess(add) { success =>
                    complete(StatusCodes.Created)
                  }
                }
              }
            }
          )
        },

        /** we need separate POST endpoint, smth like DELETE /user/$id/watchlist looks nice
          * but handles only single watchlistId
          */
        path("delete") {
          log.debug(s"POST user/$userId/watchlist/delete")
          post {
            entity(as[Watchlist]) { itemsToDelete =>
              validate(WatchlistHelper.validate(itemsToDelete), WatchlistHelper.validateMsg) {

                val delete = (watchlistActor ? DeleteItems(userId, itemsToDelete.watchlistIds)).mapTo[ActionPerformed]

                onSuccess(delete) { success =>
                  complete(StatusCodes.OK)
                }
              }
            }
          }
        }
      )
    }
}

object WatchlistHelper {
  def validate(watchlist: Watchlist): Boolean = watchlist.watchlistIds.forall(_.length == 5)

  val validateMsg = "Incorrect contentId format"
}
