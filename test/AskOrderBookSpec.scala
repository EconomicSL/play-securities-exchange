import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{Security, LimitAskOrder, AskOrderBook}
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

  val testInstrument = Security("AAPL", maxQuantity)

  feature("An AskOrderBook should maintain price priority") {

    scenario("Multiple limit ask orders are added to an empty ask order book.") {

      Given("An empty ask order book,")

      val askOrderBook = AskOrderBook(testInstrument)

      When("two limit ask orders are added to the book with the lower priced order first,")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val ask1 = LimitAskOrder(testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val ask2 = LimitAskOrder(testActor, testInstrument, highPrice, quantity2)

      askOrderBook += (ask1, ask2)

      Then("the lower priced order should be at the top of the ask order book queue.")

      askOrderBook.dequeue() should be(ask1)
      askOrderBook.dequeue() should be(ask2)
      askOrderBook.headOption should be(None)

      Given("An empty ask order book,")

      assert(askOrderBook.isEmpty)

      When("that two limit orders asks are added to the book with the higher priced order first,")

      askOrderBook += (ask2, ask1)

      Then("the lower priced order should be at the top of the ask order book queue.")

      askOrderBook.dequeue() should be(ask1)
      askOrderBook.dequeue() should be(ask2)
      askOrderBook.headOption should be(None)

    }

    scenario("An aggressive limit ask order lands in an ask order book with existing orders.") {

      val askOrderBook = AskOrderBook(testInstrument)

      Given("An ask order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingAsk1 = LimitAskOrder(testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingAsk2 = LimitAskOrder(testActor, testInstrument, highPrice, quantity2)

      askOrderBook += (existingAsk1, existingAsk2)

      When("an aggressive limit ask order lands in the book,")

      val aggressivePrice = Random.nextDouble() * lowPrice
      val quantity3 = Random.nextInt(maxQuantity)
      val aggressiveAsk = LimitAskOrder(testActor, testInstrument, aggressivePrice, quantity3)

      askOrderBook += aggressiveAsk

      Then("the aggressive limit ask order should be at the top of the ask order book queue.")

      askOrderBook.head should be(aggressiveAsk)
      askOrderBook.clear()

    }

    scenario("A passive limit ask order lands in an ask order book with existing orders.") {

      val askOrderBook = AskOrderBook(testInstrument)

      Given("An ask order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingAsk1 = LimitAskOrder(testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingAsk2 = LimitAskOrder(testActor, testInstrument, highPrice, quantity2)

      askOrderBook += (existingAsk1, existingAsk2)

      When("a passive limit ask order lands in the book,")

      val passivePrice = 0.5 * (lowPrice + highPrice)
      val quantity3 = Random.nextInt(maxQuantity)
      val passiveAsk = LimitAskOrder(testActor, testInstrument, passivePrice, quantity3)

      askOrderBook += passiveAsk

      Then("the ask order book should maintain price priority.")

      askOrderBook.dequeue() should be(existingAsk1)
      askOrderBook.dequeue() should be(passiveAsk)
      askOrderBook.dequeue() should be(existingAsk2)

    }

  }

  feature("An AskOrderBook should maintaining time priority.") {

    scenario("A limit ask order lands in an ask order book with existing orders.") {

      val askOrderBook = AskOrderBook(testInstrument)

      Given("An ask order book that contains existing orders")

      val lowPrice = Random.nextDouble() * maxPrice
      val quantity1 = Random.nextInt(maxQuantity)
      val existingAsk1 = LimitAskOrder(testActor, testInstrument, lowPrice, quantity1)

      val highPrice = (1 + Random.nextDouble()) * lowPrice
      val quantity2 = Random.nextInt(maxQuantity)
      val existingAsk2 = LimitAskOrder(testActor, testInstrument, highPrice, quantity2)

      askOrderBook +=(existingAsk1, existingAsk2)

      When("a limit ask order at the same price as the best existing limit ask order,")

      val quantity3 = Random.nextInt(maxQuantity)
      val incomingAsk = LimitAskOrder(testActor, testInstrument, lowPrice, quantity3)

      askOrderBook += incomingAsk

      Then("the best existing limit ask order should be at the top of the ask order book queue.")

      askOrderBook.dequeue() should be(existingAsk1)
      askOrderBook.dequeue() should be(incomingAsk)
      askOrderBook.dequeue() should be(existingAsk2)

    }

  }

  feature("An AskOrderBook with existing orders should have a best limit order") {

    Given("An ask order book that contains existing orders")

    val lowPrice = Random.nextDouble() * maxPrice
    val quantity1 = Random.nextInt(maxQuantity)
    val existingAsk1 = LimitAskOrder(testActor, testInstrument, lowPrice, quantity1)

    val highPrice = (1 + Random.nextDouble()) * lowPrice
    val quantity2 = Random.nextInt(maxQuantity)
    val existingAsk2 = LimitAskOrder(testActor, testInstrument, highPrice, quantity2)

    val askOrderBook = AskOrderBook(testInstrument)
    askOrderBook +=(existingAsk1, existingAsk2)

    Then("the best existing limit ask order should be at the top of the ask order book queue.")

    askOrderBook.bestLimitOrder should be(askOrderBook.headOption)
    askOrderBook.clear()
  }

  feature("An empty AskOrderBook should not have a best limit order") {

    Given("An empty ask order book")

    val askOrderBook = AskOrderBook(testInstrument)

    Then("the best existing limit ask order should be None.")

    askOrderBook.bestLimitOrder should be(None)

  }

}

