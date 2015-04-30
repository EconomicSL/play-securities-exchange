import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestProbe, TestKit, TestActorRef}
import models._
import org.scalatest.{FeatureSpecLike, GivenWhenThen, Matchers}

import scala.util.Random


class SecuritiesMarketSpec extends TestKit(ActorSystem("Securities-Market-Spec"))
  with FeatureSpecLike
  with GivenWhenThen
  with Matchers {

  def afterAll(): Unit = {
    system.shutdown()
  }

  val testInstrument = Stock("GOOG")

  def generateRandomPrice(maxPrice: Double = 1000.0): Double = {
    Random.nextDouble() * maxPrice
  }

  def generateRandomQuantity(maxQuantity: Int = 10000): Int = {
    Random.nextInt(maxQuantity)
  }

  feature("The SecuritiesMarket should be able to process orders.") {

    val seller = TestProbe()
    val buyer = TestProbe()

    val securitiesMarket = TestActorRef(new SecuritiesMarket(testInstrument))

    scenario("An incoming limit order bid crosses with the top limit order ask") {

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      securitiesMarket ! ask1

      When("a crossing limit order bid for the same quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, askQuantity)
      securitiesMarket ! bid1

      Then("the buyer should receive securities and the seller should receive payment.")

      // generate messages that should be received by seller
      val requestAssets = RequestAssets(testInstrument, ask1.quantity)
      val payment = Payment(ask1.limitPrice * ask1.quantity)

      // generate messages that should be received by buyer
      val requestPayment = RequestPayment(ask1.limitPrice * ask1.quantity)
      val assets = Assets(testInstrument, ask1.quantity)

      // tests...
      seller.expectMsg(requestAssets)
      seller.reply(assets)

      buyer.expectMsg(requestPayment)
      buyer.reply(payment)

      buyer.expectMsg(assets)
      seller.expectMsg(payment)
    }

  }

}