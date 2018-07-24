package com.example

import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.ValidationRejection
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.example.WatchlistActor.ClearStorage
import io.circe.generic.auto._
import io.circe.syntax._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

/**
  * Here we do all service integration tests as we have in-memory storage (and little time)
  * 
  * In more real-life we would just test routing here with some stubbed functionality,
  * while testing storage separately
  * so unit tests just test one layer of our business solution
  */
class UserRoutesSpec extends FunSpec with Matchers with GivenWhenThen with BeforeAndAfter
  with ScalaFutures
  with ScalatestRouteTest
  with UserRoutes {

  val watchlistActor = system.actorOf(WatchlistActor.props, "watchlistActor")

  lazy val routes = userRoutes

  after {
    watchlistActor ! ClearStorage()
  }

  def marshalEntity(watchlist: Watchlist) = Marshal(watchlist).to[MessageEntity].futureValue

  describe("A system") {
    it("should handle empty watchlist") {
      val user1 = "12345"
      Given(s"a customer with userid $user1 and an empty watchlist")
      val request1 = HttpRequest(uri = s"/user/$user1/watchlist")

      When("the customer access its watchlist")
      request1 ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)

        Then("it is empty")
        entityAs[Watchlist] should ===(Watchlist())
      }
    }
  }

  describe("A system") {
    it("should handle adding to watchlist") {
      val user1 = "12345"
      Given(s"a customer with userid $user1 and an empty watchlist")
      val request1 = HttpRequest(uri = s"/user/$user1/watchlist")

      request1 ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[Watchlist].asJson.noSpaces should ===("""{"watchlistIds":[]}""")
      }

      val contentIds = Set("crid1", "crid2", "crid3", "crid4", "crid5")
      When(s"the customer adds ContentIds $contentIds to their watchlist")
      val request2 = Post(uri = s"/user/$user1/watchlist").withEntity(marshalEntity(Watchlist(contentIds)))

      request2 ~> routes ~> check {
        status should ===(StatusCodes.Created)
      }

      Then(s"their watchlist should only containt $contentIds")
      val request3 = HttpRequest(uri = s"/user/$user1/watchlist")

      request3 ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[Watchlist] should ===(Watchlist(Set("crid1", "crid2", "crid3", "crid4", "crid5")))
      }
    }
  }

  describe("A system") {
    it("should handle adding and deleting in watchlist") {
      val user1 = "12345"
      Given(s"a customer $user1")
      val contentIds = Set("crid1", "crid2", "crid3", "crid4", "crid5")
      And(s"with a watchlist containing $contentIds")
      val request = Post(uri = s"/user/$user1/watchlist").withEntity(marshalEntity(Watchlist(contentIds)))
      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
      }
      HttpRequest(uri = s"/user/$user1/watchlist") ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[Watchlist] should ===(Watchlist(Set("crid1", "crid2", "crid3", "crid4", "crid5")))
      }
      val contentToRemove = Set("crid1")
      When(s"they remove ContentId $contentToRemove from their watchlist")
      val removeRequest = Post(uri = s"/user/$user1/watchlist/delete").withEntity(marshalEntity(Watchlist(contentToRemove)))
      removeRequest ~> routes ~> check {
        status should ===(StatusCodes.OK)
      }
      val watchlistAfter = Set("crid2", "crid3", "crid4", "crid5")
      Then(s"their watchlist should only contain $watchlistAfter")
      HttpRequest(uri = s"/user/$user1/watchlist") ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[Watchlist] should ===(Watchlist(Set("crid2", "crid3", "crid4", "crid5")))
      }
    }
  }

  describe("A system") {
    it("should handle concurrent users") {

      val user1 = "12345"
      val user2 = "54321"
      val user1Watchlist = Set("crid1", "crid2", "crid3")
      Given(s"two customers $user1 and $user2 exists in the system")
      And(s"user $user1 has a watchlist containing $user1Watchlist")
      val request1 = Post(s"/user/$user1/watchlist").withEntity(marshalEntity(Watchlist(user1Watchlist)))
      request1 ~> routes ~> check(
        status should ===(StatusCodes.Created)
      )
      val user2Watchlist = Set("crid1")
      And(s"user $user2 has a watchlist containing $user2Watchlist")
      val request2 = Post(s"/user/$user2/watchlist").withEntity(marshalEntity(Watchlist(user2Watchlist)))
      request2 ~> routes ~> check(
        status should ===(StatusCodes.Created)
      )
      When(s"customer $user1 requests their watchlist")
      Then("they should only be provided with crid1 crid2 crid3")
      val requestList = HttpRequest(uri = s"/user/$user1/watchlist")

      requestList ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[Watchlist] should ===(Watchlist(Set("crid1", "crid2", "crid3")))
      }

    }
  }

  describe("Parameter validation") {
    it("should not allow too big userIds") {
      HttpRequest(uri = "/user/123456/watchlist") ~> routes ~> check {
        handled should ===(false)
      }
    }
    it("should not allow too low userIds") {
      HttpRequest(uri = "/user/123/watchlist") ~> routes ~> check {
        handled should ===(false)
      }
    }
    it("should allow correct contentIds") {
      Post("/user/12345/watchlist").withEntity(
        Marshal(Watchlist(Set("cccc1"))).to[MessageEntity].futureValue
      ) ~> routes ~> check {
        handled should ===(true)
        status should ===(StatusCodes.Created)
      }
    }
    it("should not allow incorrect contentIds (1)") {
      Post("/user/12345/watchlist").withEntity(
        Marshal(Watchlist(Set("ccc1"))).to[MessageEntity].futureValue
      ) ~> routes ~> check {
        rejection should ===(ValidationRejection(WatchlistHelper.validateMsg, None))
      }
    }
    it("should not allow incorrect contentIds (2)") {
      Post("/user/12345/watchlist").withEntity(
        Marshal(Watchlist(Set("cccdd1"))).to[MessageEntity].futureValue
      ) ~> routes ~> check {
        rejection should ===(ValidationRejection(WatchlistHelper.validateMsg, None))
      }
    }
  }
}
