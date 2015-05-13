import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{Stock, LimitBidOrder, LimitAskOrder}
import org.scalatest.{GivenWhenThen, FeatureSpecLike, Matchers}

import scala.util.Random


class LimitAskOrderSpec extends TestKit(ActorSystem("TestSystem")) with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers {

  /** Shutdown TestSystem after running tests. */
  def afterAll(): Unit = {
    system.shutdown()
  }

  /** Maximum share price for testing. */
  val maxPrice = 1000.0

  /** Maximum number of share for testing. */
  val maxQuantity = 1e6

  val testInstrument = Stock("GOOG")

  def generateRandomQuantity(maxQuantity: Double = 1e6): Double = {
    Random.nextDouble() * maxQuantity
  }

  feature("LimitAskOrder should be able to split itself.") {

    Given("some limit ask order")

    val id = UUID.randomUUID()
    val price = Random.nextDouble() * maxPrice
    val quantity = generateRandomQuantity(maxQuantity)
    val limitAskOrder = LimitAskOrder(id, testActor, testInstrument, price, quantity)

    Then("that limit ask order should be able to split itself.")

    val newQuantity = generateRandomQuantity(quantity)
    val splitLimitAskOrder = LimitAskOrder(id, testActor, testInstrument, price, newQuantity)

    limitAskOrder.split(newQuantity) should be (splitLimitAskOrder)

  }

  feature("Crossing logic for a LimitAskOrder.") {

    scenario("Crossing a LimitAskOrder with a LimitBidOrder") {

      Given("some limit ask order")

      val askPrice = Random.nextDouble() * maxPrice
      val askQuantity = generateRandomQuantity(maxQuantity)
      val limitAskOrder = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, askPrice, askQuantity)

      Given("some limit bid order whose price exceeds that of the limit ask order")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = generateRandomQuantity(maxQuantity)
      val crossingLimitBidOrder = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, bidPrice, bidQuantity)

      Then("that limit bid order should cross with the limit ask order.")

      assert(limitAskOrder.crosses(crossingLimitBidOrder))

      Given("some limit bid order whose price does not exceed that of the limit ask order")

      val bidPrice2 = Random.nextDouble() * askPrice
      val bidQuantity2 = generateRandomQuantity(maxQuantity)
      val otherLimitBidOrder = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, bidPrice2, bidQuantity2)

      Then("that limit ask order should not cross with the limit bid order.")

      assert(! limitAskOrder.crosses(otherLimitBidOrder))

    }

  }

  feature("Price formation rules for a LimitAskOrder.") {

    scenario("Price formation between a LimitAskOrder and a LimitBidOrder") {

      Given("some limit ask order")

      val askPrice = Random.nextDouble() * maxPrice
      val askQuantity = generateRandomQuantity(maxQuantity)
      val limitAskOrder = LimitAskOrder(UUID.randomUUID(), testActor, testInstrument, askPrice, askQuantity)

      Given("some limit bid order whose price exceeds that of the limit ask order")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = generateRandomQuantity(maxQuantity)
      val crossingLimitBidOrder = LimitBidOrder(UUID.randomUUID(), testActor, testInstrument, bidPrice, bidQuantity)

      Then("the trade price should be the limit order bid price")

      limitAskOrder.formPrice(crossingLimitBidOrder) should be(bidPrice)

    }

  }

}
