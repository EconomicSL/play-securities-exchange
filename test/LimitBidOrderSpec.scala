import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{Security, LimitAskOrder, LimitBidOrder}
import org.scalatest.{GivenWhenThen, FeatureSpecLike, Matchers}

import scala.util.Random


class LimitBidOrderSpec extends TestKit(ActorSystem("TestSystem")) with
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

  feature("LimitBidOrder should be able to split itself.") {

    scenario("A LimitBidOrder needs to be split.") {

      Given("some limit bid order")

      val price = Random.nextDouble() * maxPrice
      val quantity = Random.nextInt(maxQuantity)
      val limitBidOrder = LimitBidOrder(testActor, testInstrument, price, quantity)

      Then("that limit bid order should be able to split itself.")

      val newQuantity = quantity % Random.nextInt(quantity)
      val splitLimitBidOrder = LimitBidOrder(testActor, testInstrument, price, newQuantity)

      limitBidOrder.split(newQuantity) should be (splitLimitBidOrder)
    }

  }

  feature("Crossing logic for a LimitBidOrder.") {

    scenario("Crossing a LimitBidOrder with a LimitAskOrder") {

      Given("some limit bid order")

      val bidPrice = Random.nextDouble() * maxPrice
      val bidQuantity = Random.nextInt(maxQuantity)
      val limitBidOrder = LimitBidOrder(testActor, testInstrument, bidPrice, bidQuantity)

      Given("some limit ask order whose price is less than that of the limit bid order")

      val askPrice = Random.nextDouble() * bidPrice
      val askQuantity = Random.nextInt(maxQuantity)
      val crossingLimitAskOrder = LimitAskOrder(testActor, testInstrument, askPrice, askQuantity)

      Then("that limit ask order should cross with the limit bid order.")

      limitBidOrder.crosses(crossingLimitAskOrder) should be(true)

      Given("some limit ask order whose price exceeds that of the limit bid order")

      val askPrice2 = (1 + Random.nextDouble()) * bidPrice
      val askQuantity2 = Random.nextInt(maxQuantity)
      val otherLimitAskOrder = LimitAskOrder(testActor, testInstrument, askPrice2, askQuantity2)

      Then("that limit ask order should cross with the limit bid order.")

      limitBidOrder.crosses(otherLimitAskOrder) should be(false)

    }

  }

  feature("Price formation rules for a LimitBidOrder.") {

    scenario("Price formation between a LimitBidOrder and a LimitAskOrder") {

      Given("some limit bid order")

      val bidPrice = Random.nextDouble() * maxPrice
      val bidQuantity = Random.nextInt(maxQuantity)
      val limitBidOrder = LimitBidOrder(testActor, testInstrument, bidPrice, bidQuantity)

      Given("some limit ask order whose price is less than that of the limit bid order")

      val askPrice = Random.nextDouble() * bidPrice
      val askQuantity = Random.nextInt(maxQuantity)
      val crossingLimitAskOrder = LimitAskOrder(testActor, testInstrument, askPrice, askQuantity)

      Then("the trade price should be the limit order ask price")

      limitBidOrder.formPrice(crossingLimitAskOrder) should be(askPrice)

    }

  }
}
