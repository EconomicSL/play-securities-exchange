import akka.actor.ActorSystem
import akka.testkit.TestKit
import models.{LimitBidOrder, LimitAskOrder}
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

  feature("LimitBidOrder should be able to split itself.") {

    scenario("A LimitBidOrder needs to be split.") {

      Given("some limit bid order")

      val price = Random.nextDouble() * maxPrice
      val quantity = Random.nextInt(maxQuantity)
      val limitBidOrder = LimitBidOrder(testActor, "GOOG", price, quantity)

      Then("that limit bid order should be able to split itself.")

      val newQuantity = quantity % Random.nextInt(quantity)
      val splitLimitBidOrder = LimitBidOrder(testActor, "GOOG", price, newQuantity)

      limitBidOrder.split(newQuantity) should be (splitLimitBidOrder)
    }

  }

}
