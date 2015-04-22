import akka.actor.{ActorRef, Props, ActorSystem}
import akka.testkit.{TestProbe, TestActorRef, TestKit}
import models._
import org.scalatest.{BeforeAndAfterAll, Matchers, GivenWhenThen, FeatureSpecLike}

import scala.util.Random


class BilateralClearingMechanismSpec extends TestKit(ActorSystem("NoiseTraderSpec")) with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers with
  BeforeAndAfterAll {

  /** Shutdown actor system when finished. */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  def generateRandomFill(askTradingPartyRef: ActorRef,
                         bidTradingPartyRef: ActorRef,
                         maxPrice: Double = 1e6,
                         maxQuantity: Int = 10000): FillLike = {
    val instrument = Random.nextString(4)
    val price = generateRandomPrice()
    val quantity = generateRandomQuantity()

    if (Random.nextFloat() < 0.5) {
      PartialFill(askTradingPartyRef, bidTradingPartyRef, instrument, price, quantity)
    } else {
      TotalFill(askTradingPartyRef, bidTradingPartyRef, instrument, price, quantity)
    }

  }

  def generateRandomPrice(maxPrice: Double = 1000.0): Double = {
    Random.nextDouble() * maxPrice
  }

  def generateRandomQuantity(maxQuantity: Int = 10000): Int = {
    Random.nextInt(maxQuantity)
  }

  feature("BilateralClearingMechanism should process transactions.") {

    val clearingMechanism = TestActorRef(Props[BilateralClearingMechanism])

    scenario("BilateralClearingMechanism receives a FillLike.") {

      val askTradingParty = TestProbe()
      val bidTradingParty = TestProbe()
      val partialFill = generateRandomFill(askTradingParty.ref, bidTradingParty.ref)

      When("BilateralClearingMechanism receives a FillLike")

      clearingMechanism ! partialFill

      Then("AskTradingParty should receive a request for Securities")

      val securitiesRequest = RequestSecurities(partialFill.instrument, partialFill.quantity)
      askTradingParty.expectMsg(securitiesRequest)

      Then("BidTradingParty should receive a request for Payment")

      val paymentRequest = RequestPayment(partialFill.price * partialFill.quantity)
      bidTradingParty.expectMsg(paymentRequest)

    }

  }

}
