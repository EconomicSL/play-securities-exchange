import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{LimitAskOrder, LimitBidOrder, AskOrderBook, BidOrderBook}
import org.scalatest.{BeforeAndAfterAll, FeatureSpecLike, GivenWhenThen, Matchers}

class OrderBookSpec extends TestKit(ActorSystem("OrderBookSpec")) with
  FeatureSpecLike with
  GivenWhenThen with 
  Matchers with
  BeforeAndAfterAll {

  override def afterAll(): Unit = {
    system.shutdown()
  }

  val testInstrument = "AAPL"

  feature("Adding orders to an order book") {

    scenario("Adding a single limit order to an ask order book") {

      val askBook = AskOrderBook(testInstrument)

      Given("that a single limit order ask is created and added to the book,")

      val ask = LimitAskOrder(testActor, testInstrument, 10.05, 10)
      askBook += ask

      Then("the ask book should contain that order.")

      askBook.head should be(ask)

    }

    scenario("Adding a single limit order to the bid order book") {

      val bidBook = BidOrderBook(testInstrument)

      Given("that a single limit order bid is created and added to the book,")

      val bid = LimitBidOrder(testActor, testInstrument, 10.05, 10)
      bidBook += bid

      Then("the bid book should contain that order.")

      bidBook.head should be(bid)

    }
  }

  feature("Maintaining price priority in an order book") {

    scenario("Adding multiple limit orders to the ask order book") {

      val askBook = AskOrderBook(testInstrument)

      Given("that two limit order asks are added to the book with the MORE aggressive order first,")

      val ask1 = LimitAskOrder(testActor, testInstrument, 10.6, 100)

      val ask2 = LimitAskOrder(testActor, testInstrument, 10.7, 100)

      askBook += ask1
      askBook += ask2

      Then("the ask order book should maintain price priority.")

      askBook.dequeue() should be(ask1)
      askBook.dequeue() should be(ask2)
      askBook.headOption should be(None)

      Given("that two limit orders asks are added to the book with the LESS aggressive order first,")

      askBook += ask2
      askBook += ask1

      Then("the ask order book should maintain price priority.")

      askBook.dequeue() should be(ask1)
      askBook.dequeue() should be(ask2)
      askBook.headOption should be(None)

    }

    scenario("Adding multiple limit orders to the bid order book") {

      val bidBook = BidOrderBook(testInstrument)

      Given("that two limit order bids are added to the book with the MORE aggressive order first,")
      val bid1 = LimitBidOrder(testActor, testInstrument, 10.5, 100)

      val bid2 = LimitBidOrder(testActor, testInstrument, 10.4, 100)

      bidBook +=(bid1, bid2)

      Then("the bid order book should maintain price priority.")

      bidBook.dequeue() should be(bid1)
      bidBook.dequeue() should be(bid2)
      bidBook.headOption should be(None)

      Given("that two limit order bids are received by a matching engine, with the LESS aggressive order first,")

      bidBook +=(bid2, bid1)

      Then("the bid order book should maintain price priority.")

      bidBook.dequeue() should be(bid1)
      bidBook.dequeue() should be(bid2)
      bidBook.headOption should be(None)

    }

  }
  
  feature("Maintaining time priority in an order book") {

    scenario("Adding multiple limit orders to ask order book") {

      val askBook = AskOrderBook(testInstrument)

      Given("that two limit order asks with the SAME price are added to the book,")

      val ask1 = LimitAskOrder(testActor, testInstrument, 10.7, 100)

      val ask2 = LimitAskOrder(testActor, testInstrument, 10.7, 100)

      askBook +=(ask1, ask2)

      Then("the ask order book should maintain time priority.")

      askBook.dequeue() should be(ask1)
      askBook.dequeue() should be(ask2)
      askBook.headOption should be(None)

    }

    scenario("Adding multiple limit orders to the bid order book") {

      val bidBook = BidOrderBook(testInstrument)

      Given("that two limit order bids with the SAME price are added to the book,")

      val bid1 = LimitBidOrder(testActor, testInstrument, 10.4, 100)

      val bid2 = LimitBidOrder(testActor, testInstrument, 10.4, 100)
      bidBook +=(bid1, bid2)

      Then("the bid order book should maintain time priority.")

      bidBook.dequeue() should be(bid1)
      bidBook.dequeue() should be(bid2)
      bidBook.headOption should be(None)

    }
  }
}
