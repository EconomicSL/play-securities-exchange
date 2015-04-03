import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{LimitBidOrder, BidOrderBook}
import org.scalatest.{BeforeAndAfterAll, FeatureSpecLike, GivenWhenThen, Matchers}

import scala.util.Random


class BidOrderBookSpec extends TestKit(ActorSystem("BidOrderBookSpec")) with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers with
  BeforeAndAfterAll {

  /** Shutdown actor system when finished. */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  val testInstrument = "AAPL"

  /** Maximum share price for testing. */
  val maxPrice = 1000.0

  /** Maximum number of share for testing. */
  val maxQuantity = 1000000

  feature("An BidOrderBook should maintain price priority") {

    scenario("Multiple limit bid orders are added to an empty bid order book.") {

      Given("An empty bid order book,")

      val bidOrderBook = BidOrderBook(testInstrument)

      When("two limit bid orders are added to the book with the lower priced order first,")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val bid1 = LimitBidOrder(testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val bid2 = LimitBidOrder(testActor, testInstrument, highPrice, quantity2)

      bidOrderBook += (bid1, bid2)

      Then("the higher priced order should be at the top of the bid order book queue.")

      bidOrderBook.dequeue() should be(bid2)
      bidOrderBook.dequeue() should be(bid1)
      bidOrderBook.headOption should be(None)

      Given("An empty bid order book,")

      assert(bidOrderBook.isEmpty)

      When("that two limit orders bids are added to the book with the higher priced order first,")

      bidOrderBook += (bid2, bid1)

      Then("the higher priced order should be at the top of the bid order book queue.")

      bidOrderBook.dequeue() should be(bid2)
      bidOrderBook.dequeue() should be(bid1)
      bidOrderBook.headOption should be(None)

    }

    scenario("An aggressive limit bid order lands in an bid order book with existing orders.") {

      val bidOrderBook = BidOrderBook(testInstrument)

      Given("An bid order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingBid1 = LimitBidOrder(testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingBid2 = LimitBidOrder(testActor, testInstrument, highPrice, quantity2)

      bidOrderBook += (existingBid1, existingBid2)

      When("an aggressive limit bid order lands in the book,")

      val aggressivePrice = (1 + Random.nextDouble()) * highPrice
      val quantity3 = Random.nextInt(maxQuantity)
      val aggressiveBid = LimitBidOrder(testActor, testInstrument, aggressivePrice, quantity3)

      bidOrderBook += aggressiveBid

      Then("the aggressive limit bid order should be at the top of the bid order book queue.")

      bidOrderBook.head should be(aggressiveBid)
      bidOrderBook.clear()

    }

    scenario("A passive limit bid order lands in an bid order book with existing orders.") {

      val bidOrderBook = BidOrderBook(testInstrument)

      Given("An bid order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingBid1 = LimitBidOrder(testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingBid2 = LimitBidOrder(testActor, testInstrument, highPrice, quantity2)

      bidOrderBook += (existingBid1, existingBid2)

      When("a passive limit bid order lands in the book,")

      val passivePrice = 0.5 * (lowPrice + highPrice)
      val quantity3 = Random.nextInt(maxQuantity)
      val passiveBid = LimitBidOrder(testActor, testInstrument, passivePrice, quantity3)

      bidOrderBook += passiveBid

      Then("the bid order book should maintain price priority.")

      bidOrderBook.dequeue() should be(existingBid2)
      bidOrderBook.dequeue() should be(passiveBid)
      bidOrderBook.dequeue() should be(existingBid1)

    }

  }

  feature("An BidOrderBook should maintaining time priority.") {

    scenario("A limit bid order lands in an bid order book with existing orders.") {

      val bidOrderBook = BidOrderBook(testInstrument)

      Given("An bid order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingBid1 = LimitBidOrder(testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingBid2 = LimitBidOrder(testActor, testInstrument, highPrice, quantity2)

      bidOrderBook +=(existingBid1, existingBid2)

      When("a limit bid order at the same price as the best existing limit bid order,")

      val quantity3 = Random.nextInt(maxQuantity)
      val incomingBid = LimitBidOrder(testActor, testInstrument, highPrice, quantity3)

      bidOrderBook += incomingBid

      Then("the best existing limit bid order should be at the top of the bid order book queue.")

      bidOrderBook.dequeue() should be(existingBid2)
      bidOrderBook.dequeue() should be(incomingBid)
      bidOrderBook.dequeue() should be(existingBid1)

    }

  }

  feature("An BidOrderBook with existing orders should have a best limit order") {

    Given("An bid order book that contains existing orders")

    val lowPrice = Random.nextDouble() * maxPrice
    val quantity1 = Random.nextInt(maxQuantity)
    val existingBid1 = LimitBidOrder(testActor, testInstrument, lowPrice, quantity1)

    val highPrice = (1 + Random.nextDouble()) * lowPrice
    val quantity2 = Random.nextInt(maxQuantity)
    val existingBid2 = LimitBidOrder(testActor, testInstrument, highPrice, quantity2)

    val bidOrderBook = BidOrderBook(testInstrument)
    bidOrderBook +=(existingBid1, existingBid2)

    Then("the best existing limit bid order should be at the top of the bid order book queue.")

    bidOrderBook.bestLimitOrder should be(bidOrderBook.headOption)
    bidOrderBook.clear()
  }

  feature("An empty BidOrderBook should not have a best limit order") {

    Given("An empty bid order book")

    val bidOrderBook = BidOrderBook(testInstrument)

    Then("the best existing limit bid order should be None.")

    bidOrderBook.bestLimitOrder should be(None)

  }

}

