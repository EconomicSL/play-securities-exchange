import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestProbe, TestActorRef, TestKit}
import models._
import org.scalatest.{BeforeAndAfterAll, FeatureSpecLike, GivenWhenThen, Matchers}

import scala.collection.mutable
import scala.util.Random


class NoiseTraderSpec extends TestKit(ActorSystem("NoiseTraderSpec")) with
  ImplicitSender with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers with
  BeforeAndAfterAll {

  /** Shutdown actor system when finished. */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  def generateNoiseTrader(market: ActorRef, maxCash: Double = 1e6, maxHoldings: Int =10000): NoiseTrader = {

    val assets = generateRandomAssetHoldings()
    val cash = generateRandomCashHoldings(maxCash)
    val prng = new Random()

    NoiseTrader(assets, cash, market, prng)

  }
  
  def generateRandomAmount(maxAmount: Double = 1e6): Double = {
    Random.nextDouble() * maxAmount
  }

  def generateRandomAssetHoldings(numberOfAssets: Int = 1, maxHoldings: Int = 10000): mutable.Map[String, Int] = {
    val assetHoldings = mutable.Map[String, Int]()

    for (i <- 0 until numberOfAssets) {
      assetHoldings(Random.nextString(4)) = generateRandomQuantity(maxHoldings)
    }

    assetHoldings
  }

  def generateRandomCashHoldings(maxCash: Double = 1e6): Double = {
    Random.nextDouble() * maxCash
  }

  def generateRandomPrice(maxPrice: Double = 1000.0): Double = {
    Random.nextDouble() * maxPrice
  }

  def generateRandomQuantity(maxQuantity: Int = 10000): Int = {
    Random.nextInt(maxQuantity)
  }


  feature("NoiseTrader should be able to generate new orders.") {

    val market = TestProbe()

    val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))

    val noiseTrader = noiseTraderRef.underlyingActor

    scenario("NoiseTrader wants to generate a new ask order") {

      When("NoiseTrader specifies its desired ask price")

      val askPrice = noiseTrader.decideAskPrice()

      Then("the desired ask price should be strictly positive (and less than some upper bound)")

      askPrice should (be > 0.0 and be < noiseTrader.maxPrice)

      When("NoiseTrader specifies its desired ask quantity")

      val askQuantity = noiseTrader.decideAskQuantity()

      Then("the desired ask quantity should be strictly positive (and less than some upper bound)")

      askQuantity should (be >= 1 and be <= noiseTrader.maxQuantity)

    }

    scenario("NoiseTrader wants to generate a new bid order") {

      When("NoiseTrader specifies its desired bid price.")

      val bidPrice = noiseTrader.decideBidPrice()

      Then("the desired bid price should be strictly positive (and less than some upper bound).")

      bidPrice should (be > 0.0 and be < noiseTrader.maxPrice)

      When("NoiseTrader specifies its desired bid quantity")

      val bidQuantity = noiseTrader.decideBidQuantity()

      Then("the desired bid quantity should be strictly positive (and less than some upper bound).")

      bidQuantity should (be >= 1 and be <= noiseTrader.maxQuantity)

    }

    scenario("NoiseTrader wants to generate a new order") {

      When("NoiseTrader specifies an instrument to trade")

      val instrument = noiseTrader.decideInstrument()

      Then("the desired instrument should be feasible")

      noiseTrader.assets.keySet should contain(instrument)

      When("NoiseTrader generates a new order")

      val order = noiseTrader.generateNewOrder()

      Then("the generated order has type OrderLike.")

      assert(order.isInstanceOf[OrderLike])

    }

  }

  feature("NoiseTrader should be able to send orders to the market.") {

    scenario("A NoiseTrader is created.") {

      Given("An existing NoiseTrader")

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))
      val noiseTrader = noiseTraderRef.underlyingActor

      When("A NoiseTrader receives a StartTrading message")

      noiseTraderRef ! StartTrading

      Then("the NoiseTrader should start sending orders to the market.")

      market.expectMsgType[OrderLike]

    }

    scenario("NoiseTrader should generate a new order on receipt of an OrderReceived message.") {

      Given("An existing NoiseTrader")

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))

      When("A NoiseTrader receives an OrderReceived message")

      noiseTraderRef ! OrderReceived

      Then("the NoiseTrader should send a new order to the market.")

      market.expectMsgType[OrderLike]

    }

  }

  feature("NoiseTrader should be able to process requests for payment and securities.") {

    scenario("NoiseTrader receives RequestPayment.") {

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))
      val noiseTrader =  noiseTraderRef.underlyingActor

      val initialCashHoldings = noiseTrader.cash

      When("NoiseTrader receives RequestPayment")

      val paymentRequest = RequestPayment(generateRandomAmount())
      noiseTraderRef ! paymentRequest
      
      Then("NoiseTrader decrements its cash holdings and")

      noiseTrader.cash should be (initialCashHoldings - paymentRequest.amount)

      Then("NoiseTrader sends Payment.")

      expectMsg(Payment(paymentRequest.amount))

    }

    scenario("NoiseTrader receives RequestSecurities") {

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))
      val noiseTrader =  noiseTraderRef.underlyingActor

      val symbol = noiseTrader.assets.keys.head
      val initialAssetsHoldings = noiseTrader.assets(symbol)

      When("NoiseTrader receives RequestSecurities")

      val securitiesRequest = RequestSecurities(symbol, generateRandomQuantity())
      noiseTraderRef ! securitiesRequest

      Then("NoiseTrader decrements its asset holdings and")

      noiseTrader.assets(symbol) should be (initialAssetsHoldings - securitiesRequest.quantity)

      Then("NoiseTrader sends Securities.")

      expectMsg(Securities(symbol, securitiesRequest.quantity))

    }

  }

  feature("NoiseTrader should be able to update cash (asset) holdings on receipt of Payment (Securities).") {
    
    scenario("NoiseTrader receives Payment.") {

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))
      val noiseTrader =  noiseTraderRef.underlyingActor

      val initialCashHoldings = noiseTrader.cash

      When("NoiseTrader receives Payment")

      val payment = Payment(generateRandomAmount())
      noiseTraderRef ! payment

      Then("NoiseTrader increments its cash holdings.")

      noiseTrader.cash should be (initialCashHoldings + payment.amount)

    }

    scenario("NoiseTrader receives Securities.") {

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))
      val noiseTrader =  noiseTraderRef.underlyingActor

      val symbol = noiseTrader.assets.keys.head
      val initialAssetsHoldings = noiseTrader.assets(symbol)

      When("NoiseTrader receives Securities")

      val securities = Securities(symbol, generateRandomQuantity())
      noiseTraderRef ! securities

      Then("NoiseTrader increments its asset holdings.")

      noiseTrader.assets(symbol) should be (initialAssetsHoldings + securities.quantity)

    }

  }

}
