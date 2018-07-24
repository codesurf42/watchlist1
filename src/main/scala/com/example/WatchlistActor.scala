package com.example

import akka.actor.{Actor, ActorLogging, Props}
import com.example.WatchlistActor._

import scala.collection.mutable

final case class Watchlist(watchlistIds: Set[String] = Set.empty)

class WatchlistActor extends Actor with ActorLogging {
  // We do not need concurrent map as akka actor gives us guarantee to not run it on multiple threads
  // - otherwise we would need a more clever way/proper locking to do thread-safe atomic updates
  val watchlist = new mutable.HashMap[UserId, Set[String]].empty

  def receive: Receive = {
    case ListItems(userId) =>
      log.debug(s"listItems: userId: ${userId}")
      val list = watchlist.get(userId).getOrElse(Set.empty)
      log.debug(s"userId: ${userId} Items: $list")
      sender() ! Watchlist(list)

    case AddItems(userId, newItems) =>
      log.info(s"addItems: userId: $userId new items: $newItems")
      watchlist.get(userId) match {
        case Some(existingItems) =>
          log.debug(s"addItems: userId: $userId old + new")
          watchlist.update(userId, existingItems ++ newItems)
        case None =>
          log.debug(s"addItems: userId: $userId only new: $newItems")
          watchlist.update(userId, newItems)
      }
      sender() ! ActionPerformed("items added")

    case DeleteItems(userId, itemsToDelete) =>
      log.info(s"deleteItems: userId: $userId items: $itemsToDelete")
      watchlist.get(userId).foreach { existingItems =>
        watchlist.update(userId, existingItems -- itemsToDelete)
      }
      sender() ! ActionPerformed("items deleted")

    case ClearStorage() =>
      watchlist.clear()
  }
}

object WatchlistActor {
  def props: Props = Props[WatchlistActor]

  final case class ListItems(userId: UserId)

  final case class AddItems(userId: UserId, items: Set[String])

  final case class DeleteItems(userId: UserId, items: Set[String])

  final case class ActionPerformed(description: String)

  final case class ClearStorage()

}
