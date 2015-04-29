import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestProbe, TestActorRef, TestKit}
import models._
import org.scalatest.{BeforeAndAfterAll, FeatureSpecLike, GivenWhenThen, Matchers}

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

  def generateNoiseTrader(market: ActorRef, maxCash: Double = 1e6, maxHoldings: Int = 10000): NoiseTrader = {

    val cash = generateRandomCashHoldings(maxCash)
    val prng = new Random()

    NoiseTrader(cash, market, prng)

  }
  
  def generateRandomAmount(maxAmount: Double = 1e6): Double = {
    Random.nextDouble() * maxAmount
  }

  def generateRandomAsset(symbol: String, maxQuantity: Double = 10000): Assets = {

    Assets(Stock(symbol), generateRandomQuantity(maxQuantity))

  }

  def generateRandomCashHoldings(maxCash: Double = 1e6): Double = {
    Random.nextDouble() * maxCash
  }

  def generateRandomPrice(maxPrice: Double = 1000.0): Double = {
    Random.nextDouble() * maxPrice
  }

  def generateRandomQuantity(maxQuantity: Double = 10000.0): Double = {
    Random.nextDouble() * maxQuantity
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

      askQuantity should (be >= 0.0 and be <= noiseTrader.maxQuantity)

    }

    scenario("NoiseTrader wants to generate a new bid order") {

      When("NoiseTrader specifies its desired bid price.")

      val bidPrice = noiseTrader.decideBidPrice()

      Then("the desired bid price should be strictly positive (and less than some upper bound).")

      bidPrice should (be > 0.0 and be < noiseTrader.maxPrice)

      When("NoiseTrader specifies its desired bid quantity")

      val bidQuantity = noiseTrader.decideBidQuantity()

      Then("the desired bid quantity should be strictly positive (and less than some upper bound).")

      bidQuantity should (be >= 0.0 and be <= noiseTrader.maxQuantity)

    }

    scenario("NoiseTrader wants to generate a new order") {

      val assets = generateRandomAsset("GOOG")
      noiseTraderRef ! assets

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

    scenario("NoiseTrader should generate a new order on receipt of a StartTrading message.") {

      Given("An existing NoiseTrader")

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))

      val assets = generateRandomAsset("GOOG")
      noiseTraderRef ! assets

      When("A NoiseTrader receives a StartTrading message")

      noiseTraderRef ! StartTrading

      Then("the NoiseTrader should start sending orders to the market.")

      market.expectMsgType[OrderLike]

    }

    scenario("NoiseTrader should generate a new order on receipt of an OrderReceived message.") {

      Given("An existing NoiseTrader")

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))

      val assets = generateRandomAsset("GOOG")
      noiseTraderRef ! assets

      When("A NoiseTrader receives an OrderReceived message")

      noiseTraderRef ! OrderReceived

      Then("the NoiseTrader should send a new order to the market.")

      market.expectMsgType[OrderLike]

    }

  }

  feature("NoiseTrader should be able to process requests for payment and assets.") {

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

    scenario("NoiseTrader receives RequestAssets") {

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))
      val noiseTrader =  noiseTraderRef.underlyingActor

      val initialAssetHoldings = generateRandomAsset("GOOG")
      noiseTraderRef ! initialAssetHoldings
      
      When("NoiseTrader receives RequestAssets")

      val assetsRequest = RequestAssets(initialAssetHoldings.instrument, generateRandomQuantity())
      noiseTraderRef ! assetsRequest

      Then("NoiseTrader decrements its asset holdings and")

      noiseTrader.assets(initialAssetHoldings.instrument) should be (initialAssetHoldings.quantity - assetsRequest.quantity)

      Then("NoiseTrader sends Assets.")

      expectMsg(Assets(initialAssetHoldings.instrument, assetsRequest.quantity))

    }

  }

  feature("NoiseTrader should be able to update cash (asset) holdings on receipt of Payment (Assets).") {
    
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

    scenario("NoiseTrader receives Assets.") {

      val market = TestProbe()
      val noiseTraderRef = TestActorRef(generateNoiseTrader(market.ref))
      val noiseTrader =  noiseTraderRef.underlyingActor

      val initialAssetHoldings = generateRandomAsset("GOOG")
      noiseTraderRef ! initialAssetHoldings

      When("NoiseTrader receives Assets")

      val assets = Assets(initialAssetHoldings.instrument, generateRandomQuantity())
      noiseTraderRef ! assets

      Then("NoiseTrader increments its asset holdings.")

      noiseTrader.assets(initialAssetHoldings.instrument) should be (initialAssetHoldings.quantity + assets.quantity)

    }

  }

}
