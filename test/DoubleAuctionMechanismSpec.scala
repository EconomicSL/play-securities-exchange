import akka.actor.ActorSystem
import akka.testkit.{TestProbe, TestKit, TestActorRef}
import models._
import org.scalatest.{FeatureSpecLike, GivenWhenThen, Matchers}

import scala.util.Random


class DoubleAuctionMechanismSpec extends TestKit(ActorSystem("Securities-Exchange"))
  with FeatureSpecLike
  with GivenWhenThen
  with Matchers {

  def afterAll(): Unit = {
    system.shutdown()
  }

  val testInstrument = "GOOG"

  val marketRef = TestActorRef(new DoubleAuctionMechanism(testInstrument))

  val market = marketRef.underlyingActor

  def generateRandomPrice(maxPrice: Double = 1000.0): Double = {
    Random.nextDouble() * maxPrice
  }

  def generateRandomQuantity(maxQuantity: Int = 10000): Int = {
    Random.nextInt(maxQuantity)
  }

  feature("Core matching logic for limit orders.") {

    scenario("An incoming limit order bid does not cross with the top limit order ask.") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order ask,")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      When("a limit order bid is received that does not cross the existing limit order ask")

      val bidPrice = generateRandomPrice(askPrice)
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      assert(!bid1.crosses(market.askOrderBook.head))
      marketRef ! bid1

      Then("no fills should be generated")

      // filled orders should not be generated
      seller.expectNoMsg()
      buyer.expectNoMsg()

      Then("both orders should remain on book")
      // ask order should remain on the book
      market.askOrderBook.head should be(ask1)

      // bid order should settle in the book
      market.bidOrderBook.head should be(bid1)

      // clear the books prior to next scenario!
      market.askOrderBook.clear()
      market.bidOrderBook.clear()

    }

  }

  feature("When incoming bid orders cross existing ask orders, the DoubleAuctionMechanism should generate fills.") {

    scenario("An incoming limit order bid crosses with the top limit order ask, I") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      When("a crossing limit order bid for the same quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, askQuantity)
      marketRef ! bid1

      Then("a single total fill is generated at the limit order ask price.")

      val fill = TotalFill(seller.ref, buyer.ref, testInstrument, askPrice, askQuantity)

      // expect a TotalFill to be generated
      buyer.expectMsg[TotalFill](fill)
      seller.expectMsg[TotalFill](fill)

      // both books should now be empty
      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order bid crosses with the top limit order ask, II") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      When("a crossing limit order bid for a larger quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = generateRandomQuantity() + askQuantity
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      marketRef ! bid1

      Then("a single partial fill is generated at the limit order ask price")

      val fill = PartialFill(seller.ref, buyer.ref, testInstrument, askPrice, askQuantity)

      // expect a PartialFill to be generated
      buyer.expectMsg[PartialFill](fill)
      seller.expectMsg[PartialFill](fill)

      Then("the residual bid order remains on book.")

      val residualQuantity = bidQuantity - askQuantity
      val residualBid = bid1.split(residualQuantity)
      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.head should be(residualBid)
      market.bidOrderBook.clear()

    }

    scenario("An incoming limit order bid crosses with the top limit order ask, III") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      When("a crossing limit order bid for a smaller quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = askQuantity - generateRandomQuantity(askQuantity)
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      marketRef ! bid1

      Then("a single partial fill is generated at the limit order ask price")

      val fill = PartialFill(seller.ref, buyer.ref, testInstrument, askPrice, bidQuantity)
      buyer.expectMsg[PartialFill](fill)
      seller.expectMsg[PartialFill](fill)

      Then("the residual ask order remains on book.")

      val residualQuantity = askQuantity - bidQuantity
      val residualAsk = ask1.split(residualQuantity)
      market.askOrderBook.head should be(residualAsk)
      market.bidOrderBook.headOption should be(None)
      market.askOrderBook.clear()

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, I") {

      val seller1 = TestProbe()
      val seller2 = TestProbe()
      val buyer = TestProbe()

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1.ref, testInstrument, askPrice1, askQuantity1)
      marketRef ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2.ref, testInstrument, askPrice2, askQuantity2)
      marketRef ! ask2

      When("a limit order bid for the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val bidQuantity = askQuantity1 + askQuantity2
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      marketRef ! bid1

      Then("one partial fill and one total fill should be generated")

      val fill1 = PartialFill(seller1.ref, buyer.ref, testInstrument, askPrice1, askQuantity1)
      val fill2 = TotalFill(seller2.ref, buyer.ref, testInstrument, askPrice2, askQuantity2)
      buyer.expectMsg[PartialFill](fill1)
      seller1.expectMsg[PartialFill](fill1)
      buyer.expectMsg[TotalFill](fill2)
      seller2.expectMsg[TotalFill](fill2)

      Then("both the ask and the bid order books should be empty.")

      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, II") {

      val seller1 = TestProbe()
      val seller2 = TestProbe()
      val buyer = TestProbe()

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1.ref, testInstrument, askPrice1, askQuantity1)
      marketRef ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2.ref, testInstrument, askPrice2, askQuantity2)
      marketRef ! ask2

      When("a limit order bid for more than the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val totalAskQuantity = askQuantity1 + askQuantity2
      val bidQuantity = generateRandomQuantity(totalAskQuantity) + totalAskQuantity
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      marketRef ! bid1

      Then("two partial fills should be generated")

      val fill1 = PartialFill(seller1.ref, buyer.ref, testInstrument, askPrice1, askQuantity1)
      val fill2 = PartialFill(seller2.ref, buyer.ref, testInstrument, askPrice2, askQuantity2)
      buyer.expectMsg[PartialFill](fill1)
      seller1.expectMsg[PartialFill](fill1)
      buyer.expectMsg[PartialFill](fill2)
      seller2.expectMsg[PartialFill](fill2)

      Then("the ask order book should be empty")

      market.askOrderBook.headOption should be(None)

      Then("the bid order book should contain a residual bid.")

      val residualQuantity = bidQuantity - totalAskQuantity
      val residualBid = bid1.split(residualQuantity)
      market.bidOrderBook.head should be(residualBid)
      market.bidOrderBook.clear()

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, III") {

      val seller1 = TestProbe()
      val seller2 = TestProbe()
      val buyer = TestProbe()

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1.ref, testInstrument, askPrice1, askQuantity1)
      marketRef ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2.ref, testInstrument, askPrice2, askQuantity2)
      marketRef ! ask2

      When("a limit order bid for less than the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val bidQuantity = askQuantity1 + generateRandomQuantity(askQuantity2)
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      marketRef ! bid1

      Then("two partial fills should be generated")

      val fill1 = PartialFill(seller1.ref, buyer.ref, testInstrument, askPrice1, askQuantity1)
      val residualBidQuantity = bidQuantity - askQuantity1
      val fill2 = PartialFill(seller2.ref, buyer.ref, testInstrument, askPrice2, residualBidQuantity)
      buyer.expectMsg[PartialFill](fill1)
      seller1.expectMsg[PartialFill](fill1)
      buyer.expectMsg[PartialFill](fill2)
      seller2.expectMsg[PartialFill](fill2)

      Then("the bid order book should be empty")

      market.bidOrderBook.headOption should be(None)

      Then("the ask order book should contain a residual ask.")

      val residualQuantity = askQuantity2 - residualBidQuantity
      val residualAsk = ask2.split(residualQuantity)
      market.askOrderBook.head should be(residualAsk)
      market.askOrderBook.clear()

    }

  }

  feature("When incoming ask orders cross existing bid orders, the DoubleAuctionMechanism should generate fills.") {

    scenario("An incoming limit order ask crosses with the top limit order bid, I") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      marketRef ! bid1

      When("a crossing limit order ask for the same quantity of shares is received")

      val askPrice = Random.nextDouble() * bidPrice
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, bidQuantity)
      marketRef ! ask1

      Then("a single total fill is generated at the limit order bid price.")

      val fill = TotalFill(seller.ref, buyer.ref, testInstrument, bidPrice, bidQuantity)

      // expect a TotalFill to be generated
      buyer.expectMsg[TotalFill](fill)
      seller.expectMsg[TotalFill](fill)

      // both books should now be empty
      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order ask crosses with the top limit order bid, II") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      marketRef ! bid1

      When("a crossing limit order ask for a larger quantity of shares is received")

      val askPrice = generateRandomPrice(bidPrice)
      val askQuantity = bidQuantity + generateRandomQuantity()
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      Then("a single partial fill is generated at the limit order bid price")

      val fill = PartialFill(seller.ref, buyer.ref, testInstrument, bidPrice, bidQuantity)

      // expect a PartialFill to be generated
      buyer.expectMsg[PartialFill](fill)
      seller.expectMsg[PartialFill](fill)

      Then("the residual ask order remains on book.")

      val residualQuantity = askQuantity - bidQuantity
      val residualAsk = ask1.split(residualQuantity)
      market.askOrderBook.head should be(residualAsk)
      market.bidOrderBook.headOption should be(None)
      market.askOrderBook.clear()

    }

    scenario("An incoming limit order ask crosses with the top limit order bid, III") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, bidPrice, bidQuantity)
      marketRef ! bid1

      When("a crossing limit order ask for a smaller quantity of shares is received")

      val askPrice = generateRandomPrice(bidPrice)
      val askQuantity = bidQuantity - generateRandomQuantity(bidQuantity)
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      Then("a single partial fill is generated at the limit order bid price")

      val fill = PartialFill(seller.ref, buyer.ref, testInstrument, bidPrice, askQuantity)
      buyer.expectMsg[PartialFill](fill)
      seller.expectMsg[PartialFill](fill)

      Then("the residual bid order remains on book.")

      val residualQuantity = bidQuantity - askQuantity
      val residualBid = bid1.split(residualQuantity)
      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.head should be(residualBid)
      market.bidOrderBook.clear()

    }

    scenario("An incoming limit order ask crosses multiple limit bid orders, I") {

      val buyer1 = TestProbe()
      val buyer2 = TestProbe()
      val seller = TestProbe()

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1.ref, testInstrument, bidPrice1, bidQuantity1)
      marketRef ! bid1

      val bidPrice2 = (1 + generateRandomPrice()) * bidPrice1
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2.ref, testInstrument, bidPrice2, bidQuantity2)
      marketRef ! bid2

      When("a limit order ask for the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice1)
      val askQuantity = bidQuantity1 + bidQuantity2
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      Then("one partial fill and one total fill should be generated")

      val fill1 = PartialFill(seller.ref, buyer2.ref, testInstrument, bidPrice2, bidQuantity2)
      val fill2 = TotalFill(seller.ref, buyer1.ref, testInstrument, bidPrice1, bidQuantity1)
      buyer2.expectMsg[PartialFill](fill1)
      seller.expectMsg[PartialFill](fill1)
      buyer1.expectMsg[TotalFill](fill2)
      seller.expectMsg[TotalFill](fill2)

      Then("both the ask and the bid order books should be empty.")

      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order ask crosses multiple limit ask orders, II") {

      val buyer1 = TestProbe()
      val buyer2 = TestProbe()
      val seller = TestProbe()

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1.ref, testInstrument, bidPrice1, bidQuantity1)
      marketRef ! bid1

      val bidPrice2 = (1 + generateRandomPrice()) * bidPrice1
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2.ref, testInstrument, bidPrice2, bidQuantity2)
      marketRef ! bid2

      When("a limit order ask for more than the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice1)
      val totalBidQuantity = bidQuantity1 + bidQuantity2
      val askQuantity = totalBidQuantity + generateRandomQuantity()
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      Then("two partial fills should be generated")

      val fill1 = PartialFill(seller.ref, buyer2.ref, testInstrument, bidPrice2, bidQuantity2)
      val fill2 = PartialFill(seller.ref, buyer1.ref, testInstrument, bidPrice1, bidQuantity1)
      buyer1.expectMsg[PartialFill](fill2)
      seller.expectMsg[PartialFill](fill1)
      buyer2.expectMsg[PartialFill](fill1)
      seller.expectMsg[PartialFill](fill2)

      Then("the bid order book should be empty")

      market.bidOrderBook.headOption should be(None)

      Then("the ask order book should contain a residual ask.")

      val residualQuantity = askQuantity - totalBidQuantity
      val residualAsk = ask1.split(residualQuantity)
      market.askOrderBook.head should be(residualAsk)
      market.askOrderBook.clear()

    }

    scenario("An incoming limit order ask crosses multiple limit bid orders, III") {

      val buyer1 = TestProbe()
      val buyer2 = TestProbe()
      val seller = TestProbe()

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1.ref, testInstrument, bidPrice1, bidQuantity1)
      marketRef ! bid1

      val bidPrice2 = (1 + generateRandomPrice()) * bidPrice1
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2.ref, testInstrument, bidPrice2, bidQuantity2)
      marketRef ! bid2

      When("a limit order ask for less than the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice1)
      val askQuantity = bidQuantity2 + generateRandomQuantity(bidQuantity1)
      val ask1 = LimitAskOrder(seller.ref, testInstrument, askPrice, askQuantity)
      marketRef ! ask1

      Then("two partial fills should be generated")

      val fill1 = PartialFill(seller.ref, buyer2.ref, testInstrument, bidPrice2, bidQuantity2)
      val residualAskQuantity = askQuantity - bidQuantity2
      val fill2 = PartialFill(seller.ref, buyer1.ref, testInstrument, bidPrice1, residualAskQuantity)
      buyer1.expectMsg[PartialFill](fill2)
      seller.expectMsg[PartialFill](fill1)
      buyer2.expectMsg[PartialFill](fill1)
      seller.expectMsg[PartialFill](fill2)

      Then("the ask order book should be empty")

      market.askOrderBook.headOption should be(None)

      Then("the bid order book should contain a residual bid.")

      val residualQuantity = bidQuantity1 - residualAskQuantity
      val residualBid = bid1.split(residualQuantity)
      market.bidOrderBook.head should be(residualBid)
      market.bidOrderBook.clear()

    }

  }

}