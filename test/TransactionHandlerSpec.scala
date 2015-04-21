import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe, TestKit}
import models._
import org.scalatest.{BeforeAndAfterAll, Matchers, GivenWhenThen, FeatureSpecLike}

import scala.util.Random

class TransactionHandlerSpec extends TestKit(ActorSystem("TransactionHandlerSpec")) with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers with
  BeforeAndAfterAll {

  /** Shutdown actor system when finished. */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  def generateRandomAmount(maxAmount: Double = 1e6): Double = {
    Random.nextDouble() * maxAmount
  }

  def generateRandomPrice(maxPrice: Double = 1000.0): Double = {
    Random.nextDouble() * maxPrice
  }

  def generateRandomQuantity(maxQuantity: Int = 10000): Int = {
    Random.nextInt(maxQuantity)
  }

  val testInstrument = "GOOG"


  feature("TransactionHandler should receive Fills.") {

    scenario("TransactionHandler receives a PartialFill.") {

      val transactionHandlerRef = TestActorRef(new TransactionHandler)

      val transactionHandler = transactionHandlerRef.underlyingActor

      Given("An existing PartialFill")

      val askTradingParty = TestProbe()

      val bidTradingParty = TestProbe()

      val fillPrice = generateRandomPrice()

      val fillQuantity = generateRandomQuantity()

      val partialFill = PartialFill(askTradingParty.ref,
                                    bidTradingParty.ref,
                                    testInstrument,
                                    fillPrice,
                                    fillQuantity)

      When("TransactionHandler receives a PartialFill")

      transactionHandlerRef ! partialFill

      Then("TransactionHandler should send requests for payment and securities.")

      val securitiesRequest = RequestSecurities(testInstrument, fillQuantity)
      askTradingParty.expectMsg(securitiesRequest)

      val paymentRequest = RequestPayment(fillPrice * fillQuantity)
      bidTradingParty.expectMsg(paymentRequest)

    }

    scenario("TransactionHandler receives a TotalFill.") {

      val transactionHandlerRef = TestActorRef(new TransactionHandler)

      val transactionHandler = transactionHandlerRef.underlyingActor

      Given("An existing TotalFill")

      val askTradingParty = TestProbe()

      val bidTradingParty = TestProbe()

      val fillPrice = generateRandomPrice()

      val fillQuantity = generateRandomQuantity()

      val totalFill = TotalFill(askTradingParty.ref,
        bidTradingParty.ref,
        testInstrument,
        fillPrice,
        fillQuantity)

      When("TransactionHandler receives the TotalFill")

      transactionHandlerRef ! totalFill

      Then("TransactionHandler should send requests for payment and securities.")

      val securitiesRequest = RequestSecurities(testInstrument, fillQuantity)
      askTradingParty.expectMsg(securitiesRequest)

      val paymentRequest = RequestPayment(fillPrice * fillQuantity)
      bidTradingParty.expectMsg(paymentRequest)

    }
  }

  feature("TransactionHandler should receive Payment and Securities.") {

    scenario("TransactionHandler receives a TotalFill.") {

      Given("TransactionHandler has already received a fill")

      val transactionHandlerRef = TestActorRef(new TransactionHandler)

      val transactionHandler = transactionHandlerRef.underlyingActor

      val askTradingParty = TestProbe()

      val bidTradingParty = TestProbe()

      val fillPrice = generateRandomPrice()

      val fillQuantity = generateRandomQuantity()

      val partialFill = PartialFill(askTradingParty.ref,
        bidTradingParty.ref,
        testInstrument,
        fillPrice,
        fillQuantity)

      transactionHandlerRef ! partialFill

      // confirm receipt of request for securities
      val securitiesRequest = RequestSecurities(testInstrument, fillQuantity)
      askTradingParty.expectMsg(securitiesRequest)

      // confirm receipt of request for payment
      val paymentRequest = RequestPayment(fillPrice * fillQuantity)
      bidTradingParty.expectMsgAnyOf(paymentRequest)

      When("TransactionHandler receives Payment")

      val payment = Payment(fillPrice * fillQuantity)
      transactionHandlerRef ! payment

      When("TransactionHandler receives Securities")

      val securities = Securities(testInstrument, fillQuantity)
      transactionHandlerRef ! securities

      Then("TransactionHandler should forward Payment to the seller")

      askTradingParty.expectMsg(payment)

      Then("TransactionHandler should forward Securities to the buyer")

      bidTradingParty.expectMsg(securities)

    }

  }

}

