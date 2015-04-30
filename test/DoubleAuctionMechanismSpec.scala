import akka.actor.{Props, ActorSystem}
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

  val testInstrument = Stock("GOOG")

  def generateRandomPrice(maxPrice: Double = 1000.0): Double = {
    Random.nextDouble() * maxPrice
  }

  def generateRandomQuantity(maxQuantity: Int = 10000): Int = {
    Random.nextInt(maxQuantity)
  }

  feature("Core matching logic for limit orders.") {

    val auctionMechanismRef = TestActorRef(new DoubleAuctionMechanism(testInstrument))
    val auctionMechanism = auctionMechanismRef.underlyingActor

    scenario("An incoming limit order bid does not cross with the top limit order ask.") {

      val seller = testActor
      val buyer = testActor

      Given("an existing limit order ask,")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      When("a limit order bid is received that does not cross the existing limit order ask")

      val bidPrice = generateRandomPrice(askPrice)
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      Then("both orders should remain on book")
      // ask order should remain on the book
      auctionMechanism.askOrderBook.head should be(ask1)

      // bid order should settle in the book
      auctionMechanism.bidOrderBook.head should be(bid1)

      // clear the books prior to next scenario!
      auctionMechanism.askOrderBook.clear()
      auctionMechanism.bidOrderBook.clear()

    }

    scenario("An incoming limit order bid crosses with the top limit order ask, I") {

      val seller = testActor
      val buyer = testActor

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      When("a crossing limit order bid for the same quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, askQuantity)
      auctionMechanismRef ! bid1

      Then("a single fill is generated and both order books should be empty")

      // both books should now be empty
      auctionMechanism.askOrderBook.headOption should be(None)
      auctionMechanism.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order bid crosses with the top limit order ask, II") {

      val seller = testActor
      val buyer = testActor

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      When("a crossing limit order bid for a larger quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = generateRandomQuantity() + askQuantity
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      Then("a single partial fill is generated and a residual bid order remains on book.")

      val residualQuantity = bidQuantity - askQuantity
      val residualBid = bid1.split(residualQuantity)
      auctionMechanism.askOrderBook.headOption should be(None)
      auctionMechanism.bidOrderBook.head should be(residualBid)
      auctionMechanism.bidOrderBook.clear()

    }

    scenario("An incoming limit order bid crosses with the top limit order ask, III") {

      val seller = testActor
      val buyer = testActor

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      When("a crossing limit order bid for a smaller quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = askQuantity - generateRandomQuantity(askQuantity)
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      Then("a single partial fill is generated and a residual ask order remains on book.")

      val residualQuantity = askQuantity - bidQuantity
      val residualAsk = ask1.split(residualQuantity)
      auctionMechanism.askOrderBook.head should be(residualAsk)
      auctionMechanism.bidOrderBook.headOption should be(None)
      auctionMechanism.askOrderBook.clear()

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, I") {

      val seller1 = testActor
      val seller2 = testActor
      val buyer = testActor

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1, testInstrument, askPrice1, askQuantity1)
      auctionMechanismRef ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2, testInstrument, askPrice2, askQuantity2)
      auctionMechanismRef ! ask2

      When("a limit order bid for the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val bidQuantity = askQuantity1 + askQuantity2
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      Then("both order books should be empty.")

      auctionMechanism.askOrderBook.headOption should be(None)
      auctionMechanism.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, II") {

      val seller1 = testActor
      val seller2 = testActor
      val buyer = testActor

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1, testInstrument, askPrice1, askQuantity1)
      auctionMechanismRef ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2, testInstrument, askPrice2, askQuantity2)
      auctionMechanismRef ! ask2

      When("a limit order bid for more than the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val totalAskQuantity = askQuantity1 + askQuantity2
      val bidQuantity = generateRandomQuantity(totalAskQuantity) + totalAskQuantity
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      Then("one partial fill and one total fill should be generated")

      Then("the ask order book should be empty")

      auctionMechanism.askOrderBook.headOption should be(None)

      Then("the bid order book should contain a residual bid.")

      val residualQuantity = bidQuantity - totalAskQuantity
      val residualBid = bid1.split(residualQuantity)
      auctionMechanism.bidOrderBook.head should be(residualBid)
      auctionMechanism.bidOrderBook.clear()

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, III") {

      val seller1 = testActor
      val seller2 = testActor
      val buyer = testActor

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1, testInstrument, askPrice1, askQuantity1)
      auctionMechanismRef ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2, testInstrument, askPrice2, askQuantity2)
      auctionMechanismRef ! ask2

      When("a limit order bid for less than the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val bidQuantity = askQuantity1 + generateRandomQuantity(askQuantity2)
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      Then("a single partial fill is generated at the limit order bid price")

      Then("the bid order book should be empty")

      auctionMechanism.bidOrderBook.headOption should be(None)

      Then("the ask order book should contain a residual ask.")

      val residualBidQuantity = bidQuantity - askQuantity1
      val residualQuantity = askQuantity2 - residualBidQuantity
      val residualAsk = ask2.split(residualQuantity)
      auctionMechanism.askOrderBook.head should be(residualAsk)
      auctionMechanism.askOrderBook.clear()

    }

    scenario("An incoming limit order ask crosses with the top limit order bid, I") {

      val seller = testActor
      val buyer = testActor

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      When("a crossing limit order ask for the same quantity of shares is received")

      val askPrice = Random.nextDouble() * bidPrice
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, bidQuantity)
      auctionMechanismRef ! ask1

      Then("a single total fill is generated at the limit order bid price.")

      // both books should now be empty
      auctionMechanism.askOrderBook.headOption should be(None)
      auctionMechanism.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order ask crosses with the top limit order bid, II") {

      val seller = testActor
      val buyer = testActor

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      When("a crossing limit order ask for a larger quantity of shares is received")

      val askPrice = generateRandomPrice(bidPrice)
      val askQuantity = bidQuantity + generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      Then("a single partial fill is generated and a residual ask order remains on book.")

      val residualQuantity = askQuantity - bidQuantity
      val residualAsk = ask1.split(residualQuantity)
      auctionMechanism.askOrderBook.head should be(residualAsk)
      auctionMechanism.bidOrderBook.headOption should be(None)
      auctionMechanism.askOrderBook.clear()

    }

    scenario("An incoming limit order ask crosses with the top limit order bid, III") {

      val seller = testActor
      val buyer = testActor

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      auctionMechanismRef ! bid1

      When("a crossing limit order ask for a smaller quantity of shares is received")

      val askPrice = generateRandomPrice(bidPrice)
      val askQuantity = bidQuantity - generateRandomQuantity(bidQuantity)
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      Then("a single partial fill is generated and a residual bid order remains on book.")

      val residualQuantity = bidQuantity - askQuantity
      val residualBid = bid1.split(residualQuantity)
      auctionMechanism.askOrderBook.headOption should be(None)
      auctionMechanism.bidOrderBook.head should be(residualBid)
      auctionMechanism.bidOrderBook.clear()

    }

    scenario("An incoming limit order ask crosses multiple limit bid orders, I") {

      val buyer1 = testActor
      val buyer2 = testActor
      val seller = testActor

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1, testInstrument, bidPrice1, bidQuantity1)
      auctionMechanismRef ! bid1

      val bidPrice2 = (1 + generateRandomPrice()) * bidPrice1
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2, testInstrument, bidPrice2, bidQuantity2)
      auctionMechanismRef ! bid2

      When("a limit order ask for the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice1)
      val askQuantity = bidQuantity1 + bidQuantity2
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      Then("one partial fill and one total fill should be generated")

      Then("both the ask and the bid order books should be empty.")

      auctionMechanism.askOrderBook.headOption should be(None)
      auctionMechanism.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order ask crosses multiple limit ask orders, II") {

      val buyer1 = testActor
      val buyer2 = testActor
      val seller = testActor

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1, testInstrument, bidPrice1, bidQuantity1)
      auctionMechanismRef ! bid1

      val bidPrice2 = generateRandomPrice(bidPrice1)
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2, testInstrument, bidPrice2, bidQuantity2)
      auctionMechanismRef ! bid2

      When("a limit order ask for more than the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice2)
      val totalBidQuantity = bidQuantity1 + bidQuantity2
      val askQuantity = totalBidQuantity + generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      Then("two partial fills should be generated")

      Then("the bid order book should be empty")

      auctionMechanism.bidOrderBook.headOption should be(None)

      Then("the ask order book should contain a residual ask.")

      val residualQuantity = askQuantity - totalBidQuantity
      val residualAsk = ask1.split(residualQuantity)
      auctionMechanism.askOrderBook.head should be(residualAsk)
      auctionMechanism.askOrderBook.clear()

    }

    scenario("An incoming limit order ask crosses multiple limit bid orders, III") {

      val buyer1 = testActor
      val buyer2 = testActor
      val seller = testActor

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1, testInstrument, bidPrice1, bidQuantity1)
      auctionMechanismRef ! bid1

      val bidPrice2 = generateRandomPrice(bidPrice1)
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2, testInstrument, bidPrice2, bidQuantity2)
      auctionMechanismRef ! bid2

      When("a limit order ask for less than the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice2)
      val askQuantity = bidQuantity1 + generateRandomQuantity(bidQuantity2)
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      auctionMechanismRef ! ask1

      Then("two partial fills should be generated")

      Then("the ask order book should be empty")

      auctionMechanism.askOrderBook.headOption should be(None)

      Then("the bid order book should contain a residual bid.")

      val residualAskQuantity = askQuantity - bidQuantity1
      val residualQuantity = bidQuantity2 - residualAskQuantity
      val residualBid = bid2.split(residualQuantity)
      auctionMechanism.bidOrderBook.head should be(residualBid)
      auctionMechanism.bidOrderBook.clear()

    }

  }

  feature("When incoming bid orders cross existing ask orders, the DoubleAuctionMechanism should generate fills.") {

    scenario("An incoming limit order bid crosses with the top limit order ask, I") {

      val seller = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      fabricatedMarket ! ask1

      When("a crossing limit order bid for the same quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, askQuantity)
      fabricatedMarket ! bid1

      Then("a single total fill is generated at the limit order ask price.")

      val fill = TotalFill(seller, buyer, testInstrument, askPrice, askQuantity)
      // expect a TotalFill to be generated
      securitiesMarket.expectMsg[TotalFill](fill)

    }

    scenario("An incoming limit order bid crosses with the top limit order ask, II") {

      val seller = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      fabricatedMarket ! ask1

      When("a crossing limit order bid for a larger quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = generateRandomQuantity() + askQuantity
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      fabricatedMarket ! bid1

      Then("a single partial fill is generated at the limit order ask price")

      val fill = PartialFill(seller, buyer, testInstrument, askPrice, askQuantity)

      // expect a PartialFill to be generated
      securitiesMarket.expectMsg[PartialFill](fill)

    }

    scenario("An incoming limit order bid crosses with the top limit order ask, III") {

      val seller = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("an existing limit order ask on the book")

      val askPrice = generateRandomPrice()
      val askQuantity = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      fabricatedMarket ! ask1

      When("a crossing limit order bid for a smaller quantity of shares is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice
      val bidQuantity = askQuantity - generateRandomQuantity(askQuantity)
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      fabricatedMarket ! bid1

      Then("a single partial fill is generated at the limit order ask price")

      val fill = PartialFill(seller, buyer, testInstrument, askPrice, bidQuantity)
      securitiesMarket.expectMsg[PartialFill](fill)

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, I") {

      val seller1 = testActor
      val seller2 = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1, testInstrument, askPrice1, askQuantity1)
      fabricatedMarket ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2, testInstrument, askPrice2, askQuantity2)
      fabricatedMarket ! ask2

      When("a limit order bid for the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val bidQuantity = askQuantity1 + askQuantity2
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      fabricatedMarket ! bid1

      Then("one partial fill and one total fill should be generated")

      val fill1 = PartialFill(seller1, buyer, testInstrument, askPrice1, askQuantity1)
      val fill2 = TotalFill(seller2, buyer, testInstrument, askPrice2, askQuantity2)
      securitiesMarket.expectMsg[PartialFill](fill1)
      securitiesMarket.expectMsg[TotalFill](fill2)

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, II") {

      val seller1 = testActor
      val seller2 = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1, testInstrument, askPrice1, askQuantity1)
      fabricatedMarket ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2, testInstrument, askPrice2, askQuantity2)
      fabricatedMarket ! ask2

      When("a limit order bid for more than the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val totalAskQuantity = askQuantity1 + askQuantity2
      val bidQuantity = generateRandomQuantity(totalAskQuantity) + totalAskQuantity
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      fabricatedMarket ! bid1

      Then("two partial fills should be generated")

      val fill1 = PartialFill(seller1, buyer, testInstrument, askPrice1, askQuantity1)
      val fill2 = PartialFill(seller2, buyer, testInstrument, askPrice2, askQuantity2)
      securitiesMarket.expectMsg[PartialFill](fill1)
      securitiesMarket.expectMsg[PartialFill](fill2)

    }

    scenario("An incoming limit order bid crosses multiple limit ask orders, III") {

      val seller1 = testActor
      val seller2 = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("multiple existing limit ask orders on the book")

      val askPrice1 = generateRandomPrice()
      val askQuantity1 = generateRandomQuantity()
      val ask1 = LimitAskOrder(seller1, testInstrument, askPrice1, askQuantity1)
      fabricatedMarket ! ask1

      val askPrice2 = (1 + Random.nextDouble()) * askPrice1
      val askQuantity2 = generateRandomQuantity()
      val ask2 = LimitAskOrder(seller2, testInstrument, askPrice2, askQuantity2)
      fabricatedMarket ! ask2

      When("a limit order bid for less than the total quantity of shares in both ask orders is received")

      val bidPrice = (1 + Random.nextDouble()) * askPrice2
      val bidQuantity = askQuantity1 + generateRandomQuantity(askQuantity2)
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      fabricatedMarket ! bid1

      Then("two partial fills should be generated")
      //CHECK THIS!!
      val fill1 = PartialFill(seller1, buyer, testInstrument, askPrice1, askQuantity1)
      val residualBidQuantity = bidQuantity - askQuantity1
      val fill2 = PartialFill(seller2, buyer, testInstrument, askPrice2, residualBidQuantity)
      securitiesMarket.expectMsg[PartialFill](fill1)
      securitiesMarket.expectMsg[PartialFill](fill2)

    }

  }

  feature("When incoming ask orders cross existing bid orders, the DoubleAuctionMechanism should generate fills.") {

    scenario("An incoming limit order ask crosses with the top limit order bid, I") {

      val seller = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      fabricatedMarket ! bid1

      When("a crossing limit order ask for the same quantity of shares is received")

      val askPrice = Random.nextDouble() * bidPrice
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, bidQuantity)
      fabricatedMarket ! ask1

    }

    scenario("An incoming limit order ask crosses with the top limit order bid, II") {

      val seller = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      fabricatedMarket ! bid1

      When("a crossing limit order ask for a larger quantity of shares is received")

      val askPrice = generateRandomPrice(bidPrice)
      val askQuantity = bidQuantity + generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      fabricatedMarket ! ask1

      Then("a single partial fill is generated at the limit order bid price")

      val fill = PartialFill(seller, buyer, testInstrument, bidPrice, bidQuantity)

      // expect a PartialFill to be generated
      securitiesMarket.expectMsg[PartialFill](fill)

    }

    scenario("An incoming limit order ask crosses with the top limit order bid, III") {

      val seller = testActor
      val buyer = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("an existing limit order bid on the book")

      val bidPrice = generateRandomPrice()
      val bidQuantity = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer, testInstrument, bidPrice, bidQuantity)
      fabricatedMarket ! bid1

      When("a crossing limit order ask for a smaller quantity of shares is received")

      val askPrice = generateRandomPrice(bidPrice)
      val askQuantity = bidQuantity - generateRandomQuantity(bidQuantity)
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      fabricatedMarket ! ask1

      Then("a single partial fill is generated at the limit order bid price")

      val fill = PartialFill(seller, buyer, testInstrument, bidPrice, askQuantity)
      securitiesMarket.expectMsg[PartialFill](fill)

    }

    scenario("An incoming limit order ask crosses multiple limit bid orders, I") {

      val buyer1 = testActor
      val buyer2 = testActor
      val seller = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1, testInstrument, bidPrice1, bidQuantity1)
      fabricatedMarket ! bid1

      val bidPrice2 = (1 + generateRandomPrice()) * bidPrice1
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2, testInstrument, bidPrice2, bidQuantity2)
      fabricatedMarket ! bid2

      When("a limit order ask for the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice1)
      val askQuantity = bidQuantity1 + bidQuantity2
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      fabricatedMarket ! ask1

      Then("one partial fill and one total fill should be generated")

      val fill1 = PartialFill(seller, buyer2, testInstrument, bidPrice2, bidQuantity2)
      val fill2 = TotalFill(seller, buyer1, testInstrument, bidPrice1, bidQuantity1)
      securitiesMarket.expectMsg[PartialFill](fill1)
      securitiesMarket.expectMsg[TotalFill](fill2)

    }

    scenario("An incoming limit order ask crosses multiple limit ask orders, II") {

      val buyer1 = testActor
      val buyer2 = testActor
      val seller = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1, testInstrument, bidPrice1, bidQuantity1)
      fabricatedMarket ! bid1

      val bidPrice2 = generateRandomPrice(bidPrice1)
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2, testInstrument, bidPrice2, bidQuantity2)
      fabricatedMarket ! bid2

      When("a limit order ask for more than the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice2)
      val totalBidQuantity = bidQuantity1 + bidQuantity2
      val askQuantity = totalBidQuantity + generateRandomQuantity()
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      fabricatedMarket ! ask1

      Then("two partial fills should be generated")

      val fill1 = PartialFill(seller, buyer1, testInstrument, bidPrice1, bidQuantity1)
      val fill2 = PartialFill(seller, buyer2, testInstrument, bidPrice2, bidQuantity2)
      securitiesMarket.expectMsg[PartialFill](fill1)
      securitiesMarket.expectMsg[PartialFill](fill2)

    }

    scenario("An incoming limit order ask crosses multiple limit bid orders, III") {

      val buyer1 = testActor
      val buyer2 = testActor
      val seller = testActor

      val auctionMechanismProps = DoubleAuctionMechanism.props(testInstrument)
      val securitiesMarket = TestProbe()
      val fabricatedMarket = system.actorOf(Props(new FabricatedParent(auctionMechanismProps, securitiesMarket.ref)))

      Given("multiple existing limit bid orders on the book")

      val bidPrice1 = generateRandomPrice()
      val bidQuantity1 = generateRandomQuantity()
      val bid1 = LimitBidOrder(buyer1, testInstrument, bidPrice1, bidQuantity1)
      fabricatedMarket ! bid1

      val bidPrice2 = generateRandomPrice(bidPrice1)
      val bidQuantity2 = generateRandomQuantity()
      val bid2 = LimitBidOrder(buyer2, testInstrument, bidPrice2, bidQuantity2)
      fabricatedMarket ! bid2

      When("a limit order ask for less than the total quantity of shares in both bid orders is received")

      val askPrice = generateRandomPrice(bidPrice2)
      val askQuantity = bidQuantity1 + generateRandomQuantity(bidQuantity2)
      val ask1 = LimitAskOrder(seller, testInstrument, askPrice, askQuantity)
      fabricatedMarket ! ask1

      Then("two partial fills should be generated")

      val fill1 = PartialFill(seller, buyer1, testInstrument, bidPrice1, bidQuantity1)
      val residualAskQuantity = askQuantity - bidQuantity1
      val fill2 = PartialFill(seller, buyer2, testInstrument, bidPrice2, residualAskQuantity)
      securitiesMarket.expectMsg[PartialFill](fill1)
      securitiesMarket.expectMsg[PartialFill](fill2)

    }

  }

}