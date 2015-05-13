import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{Stock, LimitAskOrder, AskOrderBook}
import org.scalatest.{BeforeAndAfterAll, FeatureSpecLike, GivenWhenThen, Matchers}

import scala.util.Random


class AskOrderBookSpec extends TestKit(ActorSystem("AskOrderBookSpec")) with
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

  feature("An AskOrderBook should maintain price priority") {

    scenario("Multiple limit ask orders are added to an empty ask order book.") {

      Given("An empty ask order book,")

      val askOrders = AskOrderBook(testInstrument)

      When("two limit ask orders are added to the book with the lower priced order first,")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val ask1 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val ask2 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

      askOrders.orderBook += (ask1, ask2)

      Then("the lower priced order should be at the top of the ask order book queue.")

      askOrders.orderBook.dequeue() should be(ask1)
      askOrders.orderBook.dequeue() should be(ask2)
      askOrders.orderBook.headOption should be(None)

      Given("An empty ask order book,")

      assert(askOrders.orderBook.isEmpty)

      When("that two limit orders asks are added to the book with the higher priced order first,")

      askOrders.orderBook += (ask2, ask1)

      Then("the lower priced order should be at the top of the ask order book queue.")

      askOrders.orderBook.dequeue() should be(ask1)
      askOrders.orderBook.dequeue() should be(ask2)
      askOrders.orderBook.headOption should be(None)

    }

    scenario("An aggressive limit ask order lands in an ask order book with existing orders.") {

      val askOrders = AskOrderBook(testInstrument)

      Given("An ask order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingAsk1 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingAsk2 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

      askOrders.orderBook += (existingAsk1, existingAsk2)

      When("an aggressive limit ask order lands in the book,")

      val aggressivePrice = Random.nextDouble() * lowPrice
      val quantity3 = Random.nextInt(maxQuantity)
      val aggressiveAsk = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, aggressivePrice, quantity3)

      askOrders.orderBook += aggressiveAsk

      Then("the aggressive limit ask order should be at the top of the ask order book queue.")

      askOrders.orderBook.head should be(aggressiveAsk)
      askOrders.orderBook.clear()

    }

    scenario("A passive limit ask order lands in an ask order book with existing orders.") {

      val askOrders = AskOrderBook(testInstrument)

      Given("An ask order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingAsk1 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingAsk2 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

      askOrders.orderBook += (existingAsk1, existingAsk2)

      When("a passive limit ask order lands in the book,")

      val passivePrice = 0.5 * (lowPrice + highPrice)
      val quantity3 = Random.nextInt(maxQuantity)
      val passiveAsk = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, passivePrice, quantity3)

      askOrders.orderBook += passiveAsk

      Then("the ask order book should maintain price priority.")

      askOrders.orderBook.dequeue() should be(existingAsk1)
      askOrders.orderBook.dequeue() should be(passiveAsk)
      askOrders.orderBook.dequeue() should be(existingAsk2)

    }

  }

  feature("An AskOrderBook should maintaining time priority.") {

    scenario("A limit ask order lands in an ask order book with existing orders.") {

      val askOrders = AskOrderBook(testInstrument)

      Given("An ask order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingAsk1 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingAsk2 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

      askOrders.orderBook +=(existingAsk1, existingAsk2)

      When("a limit ask order at the same price as the best existing limit ask order,")

      val quantity3 = Random.nextInt(maxQuantity)
      val incomingAsk = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity3)

      askOrders.orderBook += incomingAsk

      Then("the best existing limit ask order should be at the top of the ask order book queue.")

      askOrders.orderBook.dequeue() should be(existingAsk1)
      askOrders.orderBook.dequeue() should be(incomingAsk)
      askOrders.orderBook.dequeue() should be(existingAsk2)

    }

  }

  feature("An AskOrderBook with existing orders should have a best limit order") {

    Given("An ask order book that contains existing orders")

    val lowPrice = Random.nextDouble() * maxPrice
    val quantity1 = Random.nextInt(maxQuantity)
    val existingAsk1 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, lowPrice, quantity1)

    val highPrice = (1 + Random.nextDouble()) * lowPrice
    val quantity2 = Random.nextInt(maxQuantity)
    val existingAsk2 = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, highPrice, quantity2)

    val askOrders = AskOrderBook(testInstrument)
    askOrders.orderBook += (existingAsk1, existingAsk2)

    Then("the best existing limit ask order should be at the top of the ask order book queue.")

    askOrders.bestLimitOrder should be(askOrders.orderBook.headOption)
    askOrders.orderBook.clear()
  }

  feature("An empty AskOrderBook should not have a best limit order") {

    Given("An empty ask order book")

    val askOrders = AskOrderBook(testInstrument)

    Then("the best existing limit ask order should be None.")

    askOrders.bestLimitOrder should be(None)

  }

}

