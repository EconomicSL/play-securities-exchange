import akka.actor.{Props, ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe, TestKit}
import models._
import org.scalatest.{BeforeAndAfterAll, Matchers, GivenWhenThen, FeatureSpecLike}

import scala.collection.mutable
import scala.util.Random


class CCPClearingMechanismSpec extends TestKit(ActorSystem("CCPClearingMechanismSpec")) with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers with
  BeforeAndAfterAll {

  /** Shutdown actor system when finished. */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  def generateNoiseTrader(market: ActorRef, instruments: Seq[String], maxCash: Double = 1e6, maxHoldings: Int =10000): NoiseTrader = {

    val assets = generateRandomAssetHoldings(instruments, maxHoldings)
    val cash = generateRandomCashHoldings(maxCash)
    val prng = new Random()

    NoiseTrader(assets, cash, market, prng)

  }

  def generateRandomAssetHoldings(instruments: Seq[String], maxHoldings: Int = 10000): mutable.Map[String, Int] = {
    val assetHoldings = mutable.Map[String, Int]()

    for (instrument <- instruments) {
      assetHoldings(instrument) = generateRandomQuantity(maxHoldings)
    }

    assetHoldings
  }

  def generateRandomCashHoldings(maxCash: Double = 1e6): Double = {
    Random.nextDouble() * maxCash
  }

  def generateRandomPartialFill(askTradingPartyRef: ActorRef,
                                bidTradingPartyRef: ActorRef,
                                instrument: String,
                                maxPrice: Double = 1e6,
                                maxQuantity: Int = 10000): FillLike = {
    val price = generateRandomPrice()
    val quantity = generateRandomQuantity()

    PartialFill(askTradingPartyRef, bidTradingPartyRef, instrument, price, quantity)
  }

  def generateRandomTotalFill(askTradingPartyRef: ActorRef,
                              bidTradingPartyRef: ActorRef,
                              instrument: String,
                              maxPrice: Double = 1e6,
                              maxQuantity: Int = 10000): FillLike = {

    val price = generateRandomPrice()
    val quantity = generateRandomQuantity()

    TotalFill(askTradingPartyRef, bidTradingPartyRef, instrument, price, quantity)

  }

  def generateRandomPrice(maxPrice: Double = 1000.0): Double = {
    Random.nextDouble() * maxPrice
  }

  def generateRandomQuantity(maxQuantity: Int = 10000): Int = {
    Random.nextInt(maxQuantity)
  }


  feature("CCPClearingMechanism should process transactions.") {

    val clearingMechanismRef = TestActorRef(new CCPClearingMechanism)
    val clearingMechanism = clearingMechanismRef.underlyingActor

    val askTradingParty = TestProbe()
    val bidTradingParty = TestProbe()

    scenario("CCPClearingMechanism receives a PartialFill.") {

      val fill = generateRandomPartialFill(askTradingParty.ref, bidTradingParty.ref, "APPL")

      // store initial holdings of cash and securities
      val clearingMechanismInitialSecurities = clearingMechanism.securities(fill.instrument)
      val clearingMechanismInitialCash = clearingMechanism.cash

      When("CCPClearingMechanism receives a PartialFill")

      clearingMechanismRef ! fill

      Then("AskTradingParty should receive a request for Securities")

      askTradingParty.expectMsg(RequestSecurities(fill.instrument, fill.quantity))
      askTradingParty.reply(Securities(fill.instrument, fill.quantity))

      Then("BidTradingParty should receive a request for Payment")

      val paymentAmount = fill.price * fill.quantity
      bidTradingParty.expectMsg(RequestPayment(paymentAmount))
      bidTradingParty.reply(Payment(paymentAmount))

      Then("AskTradingParty should receive a Payment")

      askTradingParty.expectMsg(Payment(paymentAmount))

      Then("BidTradingParty should receive a Securities")

      bidTradingParty.expectMsg(Securities(fill.instrument, fill.quantity))

      Then("CCPClearingMechanism securities holdings should remain unchanged.")

      clearingMechanism.securities(fill.instrument) should be(clearingMechanismInitialSecurities)

      Then("CCPClearingMechanism cash holdings should remain unchanged.")

      clearingMechanism.cash should be(clearingMechanismInitialCash)

    }

    scenario("CCPClearingMechanism receives a TotalFill.") {

      val fill = generateRandomPartialFill(askTradingParty.ref, bidTradingParty.ref, "APPL")

      // store initial holdings of cash and securities
      val clearingMechanismInitialSecurities = clearingMechanism.securities(fill.instrument)
      val clearingMechanismInitialCash = clearingMechanism.cash

      When("CCPClearingMechanism receives a PartialFill")

      clearingMechanismRef ! fill

      Then("AskTradingParty should receive a request for Securities")

      askTradingParty.expectMsg(RequestSecurities(fill.instrument, fill.quantity))
      askTradingParty.reply(Securities(fill.instrument, fill.quantity))

      Then("BidTradingParty should receive a request for Payment")

      val paymentAmount = fill.price * fill.quantity
      bidTradingParty.expectMsg(RequestPayment(paymentAmount))
      bidTradingParty.reply(Payment(paymentAmount))

      Then("AskTradingParty should receive a Payment")

      askTradingParty.expectMsg(Payment(paymentAmount))

      Then("BidTradingParty should receive a Securities")

      bidTradingParty.expectMsg(Securities(fill.instrument, fill.quantity))

      Then("CCPClearingMechanism securities holdings should remain unchanged.")

      clearingMechanism.securities(fill.instrument) should be(clearingMechanismInitialSecurities)

      Then("CCPClearingMechanism cash holdings should remain unchanged.")

      clearingMechanism.cash should be(clearingMechanismInitialCash)

    }

  }

}

