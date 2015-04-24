import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{Security, LimitBidOrder, LimitAskOrder}
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
  val maxQuantity = 1000000

  val testInstrument = Security("GOOG", maxQuantity)

  feature("LimitAskOrder should be able to split itself.") {

    Given("some limit ask order")

    val price = Random.nextDouble() * maxPrice
    val quantity = Random.nextInt(maxQuantity)
    val limitAskOrder = LimitAskOrder(testActor, testInstrument, price, quantity)

    Then("that limit ask order should be able to split itself.")

    val newQuantity = quantity % Random.nextInt(quantity)
    val splitLimitAskOrder = LimitAskOrder(testActor, testInstrument, price, newQuantity)

    limitAskOrder.split(newQuantity) should be (splitLimitAskOrder)

  }

  feature("Crossing logic for a LimitAskOrder.") {

    scenario("Crossing a LimitAskOrder with a LimitBidOrder") {

      Given("some limit ask order")

      val askPrice = Random.nextDouble() * maxPrice
      val askQuantity = Random.nextInt(maxQuantity)
      val limitAskOrder = LimitAskOrder(testActor, testInstrument, askPrice, askQuantity)

      Given("some limit bid order whose price exceeds that of the limit ask order")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = Random.nextInt(maxQuantity)
      val crossingLimitBidOrder = LimitBidOrder(testActor, testInstrument, bidPrice, bidQuantity)

      Then("that limit bid order should cross with the limit ask order.")

      limitAskOrder.crosses(crossingLimitBidOrder) should be(true)

      Given("some limit bid order whose price does not exceed that of the limit ask order")

      val bidPrice2 = Random.nextDouble() * askPrice
      val bidQuantity2 = Random.nextInt(maxQuantity)
      val otherLimitBidOrder = LimitBidOrder(testActor, testInstrument, bidPrice2, bidQuantity2)

      Then("that limit ask order should cross with the limit bid order.")

      limitAskOrder.crosses(otherLimitBidOrder) should be(false)

    }

  }

  feature("Price formation rules for a LimitAskOrder.") {

    scenario("Price formation between a LimitAskOrder and a LimitBidOrder") {

      Given("some limit ask order")

      val askPrice = Random.nextDouble() * maxPrice
      val askQuantity = Random.nextInt(maxQuantity)
      val limitAskOrder = LimitAskOrder(testActor, testInstrument, askPrice, askQuantity)

      Given("some limit bid order whose price exceeds that of the limit ask order")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = Random.nextInt(maxQuantity)
      val crossingLimitBidOrder = LimitBidOrder(testActor, testInstrument, bidPrice, bidQuantity)

      Then("the trade price should be the limit order bid price")

      limitAskOrder.formPrice(crossingLimitBidOrder) should be(bidPrice)

    }

  }

}
