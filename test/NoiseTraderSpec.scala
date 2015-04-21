import akka.actor.ActorSystem
import akka.testkit.{TestProbe, TestActorRef, TestKit}
import models._
import org.scalatest.{BeforeAndAfterAll, FeatureSpecLike, GivenWhenThen, Matchers}

import scala.collection.mutable
import scala.util.Random


class NoiseTraderSpec extends TestKit(ActorSystem("NoiseTraderSpec")) with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers with
  BeforeAndAfterAll {

  /** Shutdown actor system when finished. */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  feature("NoiseTrader should be able to generate new orders.") {

    val assets = mutable.Map[String, Int](("APPL", 10000))

    val cash = Double.PositiveInfinity

    val marketRef = testActor

    val prng = new Random()

    val noiseTraderRef = TestActorRef(new NoiseTrader(assets, cash, marketRef, prng))

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

    val assets = mutable.Map[String, Int](("APPL", 10000))

    val cash = Double.PositiveInfinity

    val market = TestProbe()

    val prng = new Random()

    scenario("NoiseTrader should generate a new order on receipt of a StartTrading message.") {

      Given("An existing NoiseTrader")

      val noiseTraderRef = TestActorRef(new NoiseTrader(assets, cash, market.ref, prng))

      When("A NoiseTrader receives a StartTrading message")

      noiseTraderRef ! StartTrading

      Then("the NoiseTrader should start sending orders to the market.")

      market.expectMsgType[OrderLike]

    }

    scenario("NoiseTrader should generate a new order on receipt of an OrderReceived message.") {

      Given("An existing NoiseTrader")

      val noiseTraderRef = TestActorRef(new NoiseTrader(assets, cash, market.ref, prng))

      When("A NoiseTrader receives an OrderReceived message")

      noiseTraderRef ! OrderReceived

      Then("the NoiseTrader should send a new order to the market.")

      market.expectMsgType[OrderLike]

    }

  }

}
