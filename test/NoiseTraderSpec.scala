import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit}
import models.{BidOrderLike, AskOrderLike, NoiseTrader, DoubleAuctionMechanism}
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

  /* Create an instance of a Market actor. */
  val testInstrument = "AAPL"

  val marketRef = TestActorRef(new DoubleAuctionMechanism(testInstrument))

  val market = marketRef.underlyingActor

  /* Create an instance of a NoiseTrader for testing. */
  val assets = mutable.Map[String, Int]((testInstrument, 0))

  val cash = Double.PositiveInfinity

  val prng = new Random()

  val noiseTraderRef = TestActorRef(new NoiseTrader(assets, cash, marketRef, prng))

  val noiseTrader = noiseTraderRef.underlyingActor

  feature("NoiseTrader should be able to specify a desired ask price.") {

    noiseTrader.decideAskPrice() should (be >= 0.0 and be <= noiseTrader.maxPrice)

  }

  feature("NoiseTrader should be able to specify a desired bid price.") {

    noiseTrader.decideBidPrice() should (be >= 0.0 and be <= noiseTrader.maxPrice)

  }

  feature("NoiseTrader should be able to specify a desired ask quantity.") {

    noiseTrader.decideAskQuantity() should (be >= 1 and be <= noiseTrader.maxQuantity)

  }

  feature("NoiseTrader should be able to specify a desired bid quantity.") {

    noiseTrader.decideBidQuantity() should (be >= 1 and be <= noiseTrader.maxQuantity)

  }

  feature("NoiseTrader should be able to specify an instrument to trade.") {

    noiseTrader.assets.keySet should contain (noiseTrader.decideInstrument())

  }

  feature("NoiseTrader should be able to generate new ask orders.") {

    assert(noiseTrader.generateNewAskOrder().isInstanceOf[AskOrderLike])

  }

  feature("NoiseTrader should be able to specify an instrument to trade.") {

    assert(noiseTrader.generateNewBidOrder().isInstanceOf[BidOrderLike])

  }

}
