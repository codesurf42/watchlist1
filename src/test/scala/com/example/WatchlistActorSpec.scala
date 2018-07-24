package com.example

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.example.WatchlistActor.{ActionPerformed, AddItems, DeleteItems, ListItems}
import org.scalatest.{BeforeAndAfterAll, FunSpecLike}
import org.scalatest.concurrent.ScalaFutures

class WatchlistActorSpec extends TestKit(ActorSystem()) with ImplicitSender
  with FunSpecLike with BeforeAndAfterAll
  with ScalaFutures {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
  
  describe("A watchlist actor") {
    it("should handle deletes on empty list") {
      val test = system.actorOf(WatchlistActor.props)

      ignoreMsg { case ActionPerformed(_) => true }

      test ! DeleteItems(UserId(0), Set("a", "b", "c"))

      test ! ListItems(UserId(0))
      expectMsg(Watchlist())

      test ! AddItems(UserId(0), Set("a"))
      test ! DeleteItems(UserId(0), Set("b", "c"))
      test ! ListItems(UserId(0))
      expectMsg(Watchlist(Set("a")))
    }
  }

}
