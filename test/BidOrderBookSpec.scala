import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{Stock, LimitBidOrder, BidOrderBook}
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

  /** Maximum share price for testing. */
  val maxPrice = 1000.0

  /** Maximum number of share for testing. */
  val maxQuantity = 1000000

  val testInstrument = Stock("AAPL")

  feature("An BidOrderBook should maintain price priority") {

    scenario("Multiple limit bid orders are added to an empty bid order book.") {

      Given("An empty bid order book,")

      val bidOrders = BidOrderBook(testInstrument)

      When("two limit bid orders are added to the book with the lower priced order first,")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val bid1 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val bid2 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

      bidOrders.orderBook += (bid1, bid2)

      Then("the higher priced order should be at the top of the bid order book queue.")

      bidOrders.orderBook.dequeue() should be(bid2)
      bidOrders.orderBook.dequeue() should be(bid1)
      bidOrders.orderBook.headOption should be(None)

      Given("An empty bid order book,")

      assert(bidOrders.orderBook.isEmpty)

      When("that two limit orders bids are added to the book with the higher priced order first,")

      bidOrders.orderBook += (bid2, bid1)

      Then("the higher priced order should be at the top of the bid order book queue.")

      bidOrders.orderBook.dequeue() should be(bid2)
      bidOrders.orderBook.dequeue() should be(bid1)
      bidOrders.orderBook.headOption should be(None)

    }

    scenario("An aggressive limit bid order lands in an bid order book with existing orders.") {

      val bidOrders = BidOrderBook(testInstrument)

      Given("An bid order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingBid1 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingBid2 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

      bidOrders.orderBook += (existingBid1, existingBid2)

      When("an aggressive limit bid order lands in the book,")

      val aggressivePrice = (1 + Random.nextDouble()) * highPrice
      val quantity3 = Random.nextInt(maxQuantity)
      val aggressiveBid = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, aggressivePrice, quantity3)

      bidOrders.orderBook += aggressiveBid

      Then("the aggressive limit bid order should be at the top of the bid order book queue.")

      bidOrders.orderBook.head should be(aggressiveBid)
      bidOrders.orderBook.clear()

    }

    scenario("A passive limit bid order lands in an bid order book with existing orders.") {

      val bidOrders = BidOrderBook(testInstrument)

      Given("An bid order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingBid1 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingBid2 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

      bidOrders.orderBook += (existingBid1, existingBid2)

      When("a passive limit bid order lands in the book,")

      val passivePrice = 0.5 * (lowPrice + highPrice)
      val quantity3 = Random.nextInt(maxQuantity)
      val passiveBid = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, passivePrice, quantity3)

      bidOrders.orderBook += passiveBid

      Then("the bid order book should maintain price priority.")

      bidOrders.orderBook.dequeue() should be(existingBid2)
      bidOrders.orderBook.dequeue() should be(passiveBid)
      bidOrders.orderBook.dequeue() should be(existingBid1)

    }

  }

  feature("An BidOrderBook should maintaining time priority.") {

    scenario("A limit bid order lands in an bid order book with existing orders.") {

      val bidOrders = BidOrderBook(testInstrument)

      Given("An bid order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingBid1 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingBid2 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

      bidOrders.orderBook +=(existingBid1, existingBid2)

      When("a limit bid order at the same price as the best existing limit bid order,")

      val quantity3 = Random.nextInt(maxQuantity)
      val incomingBid = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity3)

      bidOrders.orderBook += incomingBid

      Then("the best existing limit bid order should be at the top of the bid order book queue.")

      bidOrders.orderBook.dequeue() should be(existingBid2)
      bidOrders.orderBook.dequeue() should be(incomingBid)
      bidOrders.orderBook.dequeue() should be(existingBid1)

    }

  }

  feature("An BidOrderBook with existing orders should have a best limit order") {

    Given("An bid order book that contains existing orders")

    val lowPrice = Random.nextDouble() * maxPrice
    val quantity1 = Random.nextInt(maxQuantity)
    val existingBid1 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

    val highPrice = (1 + Random.nextDouble()) * lowPrice
    val quantity2 = Random.nextInt(maxQuantity)
    val existingBid2 = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

    val bidOrders = BidOrderBook(testInstrument)
    bidOrders.orderBook +=(existingBid1, existingBid2)

    Then("the best existing limit bid order should be at the top of the bid order book queue.")

    bidOrders.bestLimitOrder should be(bidOrders.orderBook.headOption)
    bidOrders.orderBook.clear()
  }

  feature("An empty BidOrderBook should not have a best limit order") {

    Given("An empty bid order book")

    val bidOrders = BidOrderBook(testInstrument)

    Then("the best existing limit bid order should be None.")

    bidOrders.bestLimitOrder should be(None)

  }

}

