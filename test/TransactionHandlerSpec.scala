import akka.actor.{ActorRef, ActorSystem}
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

  def generateRandomPartialFill(askTradingPartyRef: ActorRef,
                                bidTradingPartyRef: ActorRef,
                                maxPrice: Double = 1e6,
                                maxQuantity: Int = 10000): PartialFill = {
    val instrument = Random.nextString(4)
    val price = generateRandomPrice()
    val quantity = generateRandomQuantity()

    PartialFill(askTradingPartyRef, bidTradingPartyRef, instrument, price, quantity)
  }

  def generateRandomTotalFill(askTradingPartyRef: ActorRef,
                              bidTradingPartyRef: ActorRef,
                              maxPrice: Double = 1e6,
                              maxQuantity: Int = 10000): TotalFill = {

    val instrument = Random.nextString(4)
    val price = generateRandomPrice()
    val quantity = generateRandomQuantity()

    TotalFill(askTradingPartyRef, bidTradingPartyRef, instrument, price, quantity)

  }

  feature("TransactionHandler should receive Fills.") {

    scenario("TransactionHandler receives a PartialFill.") {

      val transactionHandlerRef = TestActorRef(new TransactionHandler)

      Given("An existing PartialFill")

      val askTradingParty = TestProbe()
      val bidTradingParty = TestProbe()
      val fill = generateRandomPartialFill(askTradingParty.ref, bidTradingParty.ref)

      When("TransactionHandler receives a PartialFill")

      transactionHandlerRef ! fill

      Then("TransactionHandler should send requests for payment and securities.")

      val securitiesRequest = RequestSecurities(fill.instrument, fill.quantity)
      askTradingParty.expectMsg(securitiesRequest)

      val paymentRequest = RequestPayment(fill.price * fill.quantity)
      bidTradingParty.expectMsg(paymentRequest)

    }

    scenario("TransactionHandler receives a TotalFill.") {

      val transactionHandlerRef = TestActorRef(new TransactionHandler)

      Given("An existing TotalFill")

      val askTradingParty = TestProbe()
      val bidTradingParty = TestProbe()
      val fill = generateRandomTotalFill(askTradingParty.ref, bidTradingParty.ref)

      When("TransactionHandler receives the TotalFill")

      transactionHandlerRef ! fill

      Then("TransactionHandler should send requests for payment and securities.")

      val securitiesRequest = RequestSecurities(fill.instrument, fill.quantity)
      askTradingParty.expectMsg(securitiesRequest)

      val paymentRequest = RequestPayment(fill.price * fill.quantity)
      bidTradingParty.expectMsg(paymentRequest)

    }
  }

  feature("TransactionHandler should receive Payment and Securities.") {

    scenario("TransactionHandler receives Payment before Securities.") {

      val transactionHandlerRef = TestActorRef(new TransactionHandler)
      val transactionHandler = transactionHandlerRef.underlyingActor

      Given("TransactionHandler has already received a fill")

      val askTradingParty = TestProbe()
      val bidTradingParty = TestProbe()
      val fill = generateRandomPartialFill(askTradingParty.ref, bidTradingParty.ref)

      transactionHandlerRef ! fill

      When("TransactionHandler receives Payment")

      val payment = Payment(fill.price * fill.quantity)
      transactionHandlerRef ! payment
      assert(transactionHandler.paymentReceived)

      When("TransactionHandler receives Securities")

      val securities = Securities(fill.instrument, fill.quantity)
      transactionHandlerRef ! securities
      assert(transactionHandler.securitiesReceived)

      Then("TransactionHandler should forward Payment to the seller")

      val securitiesRequest = RequestSecurities(fill.instrument, fill.quantity)
      askTradingParty.expectMsg(securitiesRequest)
      askTradingParty.expectMsg(payment)

      Then("TransactionHandler should forward Securities to the buyer")

      val paymentRequest = RequestPayment(fill.price * fill.quantity)
      bidTradingParty.expectMsg(paymentRequest)
      bidTradingParty.expectMsg(securities)

    }

    scenario("TransactionHandler receives Securities before Payment.") {

      val transactionHandlerRef = TestActorRef(new TransactionHandler)
      val transactionHandler = transactionHandlerRef.underlyingActor

      Given("TransactionHandler has already received a fill")

      val askTradingParty = TestProbe()
      val bidTradingParty = TestProbe()
      val fill = generateRandomPartialFill(askTradingParty.ref, bidTradingParty.ref)

      transactionHandlerRef ! fill

      When("TransactionHandler receives Securities")

      val securities = Securities(fill.instrument, fill.quantity)
      transactionHandlerRef ! securities
      assert(transactionHandler.securitiesReceived)

      When("TransactionHandler receives Payment")

      val payment = Payment(fill.price * fill.quantity)
      transactionHandlerRef ! payment
      assert(transactionHandler.paymentReceived)

      Then("TransactionHandler should forward Payment to the seller")

      val securitiesRequest = RequestSecurities(fill.instrument, fill.quantity)
      askTradingParty.expectMsg(securitiesRequest)
      askTradingParty.expectMsg(payment)

      Then("TransactionHandler should forward Securities to the buyer")

      val paymentRequest = RequestPayment(fill.price * fill.quantity)
      bidTradingParty.expectMsg(paymentRequest)
      bidTradingParty.expectMsg(securities)

    }

  }

}

