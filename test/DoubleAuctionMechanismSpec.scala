import akka.actor.ActorSystem
import akka.testkit.{TestProbe, TestKit, TestActorRef}
import models._
import org.scalatest.{FeatureSpecLike, GivenWhenThen, Matchers}


class DoubleAuctionMechanismSpec extends TestKit(ActorSystem("Securities-Exchange"))
  with FeatureSpecLike
  with GivenWhenThen
  with Matchers {

  val testInstrument = "GOOG"

  val marketRef = TestActorRef(new DoubleAuctionMechanism(testInstrument))

  val market = marketRef.underlyingActor

  def afterAll(): Unit = {
    system.shutdown()
  }

  feature("Core matching logic for limit orders.") {

    scenario("An incoming limit order bid does not cross with the top limit order ask.") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order ask,")

      val ask1 = LimitAskOrder(seller.ref, testInstrument, 10.6, 100)

      market.askOrderBook += ask1

      When("a limit order bid is received that does not cross the existing limit order ask")

      val bid1 = LimitBidOrder(buyer.ref, testInstrument, 10.5, 20)

      marketRef ! bid1

      Then("no trades are generated and both orders should remain on book.")

      // filled orders should not be generated
      seller.expectNoMsg()
      buyer.expectNoMsg()

      // ask order should remain on the book
      market.askOrderBook.head should be(ask1)

      // bid order should settle in the book
      market.bidOrderBook.head should be(bid1)

      // clear the books prior to next scenario!
      market.askOrderBook.clear()
      market.bidOrderBook.clear()

    }

    scenario("An incoming limit order bid crosses with the top limit order ask.") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order ask on the book for 100 shares")

      val ask1 = LimitAskOrder(seller.ref, testInstrument, 10.4, 100)

      market.askOrderBook += ask1


      When("a crossing limit order bid for 100 shares is received,")

      val bid1 = LimitBidOrder(buyer.ref, testInstrument, 10.5, 100)

      marketRef ! bid1

      Then("a single total fill is generated at the limit order ask price.")

      val trade = TotalFill(seller.ref, buyer.ref, testInstrument,
        price = 10.4,
        quantity = 100)

      // expect a TotalFill to be generated
      buyer.expectMsg[TotalFill](trade)
      seller.expectMsg[TotalFill](trade)

      // both books should now be empty
      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.headOption should be(None)

    }

    scenario("An incoming limit order ask crosses with the top limit order bid.") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("an existing limit order bid for 100 shares on the book")

      val bid1 = LimitBidOrder(buyer.ref, testInstrument, 10.4, 100)

      market.bidOrderBook += bid1

      When("a crossing limit order ask for 100 shares is received,")

      val ask1 = LimitAskOrder(seller.ref, testInstrument, 10.2, 100)

      marketRef ! ask1

      Then("a single total fill is generated at the limit order bid price.")

      val trade = TotalFill(seller.ref, buyer.ref, testInstrument, 10.4, 100)

      // expect a TotalFill to be generated
      buyer.expectMsg[TotalFill](trade)
      seller.expectMsg[TotalFill](trade)

      // both books should now be empty
      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.headOption should be(None)

    }

    scenario("A limit order ask is filled by multiple limit order bids.") {

      val seller = TestProbe()
      val buyer1 = TestProbe()
      val buyer2 = TestProbe()

      Given("two small limit order bids for 100 shares in total on the book")

      val bid1 = LimitBidOrder(buyer1.ref, testInstrument, 10.5, 20)

      val bid2 = LimitBidOrder(buyer2.ref, testInstrument, 10.6, 80)

      market.bidOrderBook +=(bid1, bid2)

      When("a crossing limit order ask for 100 shares is received,")

      val ask1 = LimitAskOrder(seller.ref, testInstrument, 10.5, 100)

      marketRef ! ask1

      Then("the limit order ask should be filled in two separate trades.")

      val trade1 = PartialFill(seller.ref, buyer2.ref, testInstrument, 10.6, 80)
      val trade2 = TotalFill(seller.ref, buyer1.ref, testInstrument, 10.5, 20)

      // each buyer receives a single message
      buyer2.expectMsg[PartialFill](trade1)
      buyer1.expectMsg[TotalFill](trade2)

      // seller should get two messages
      seller.expectMsg[PartialFill](trade1)
      seller.expectMsg[TotalFill](trade2)

      // both books should now be empty
      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.headOption should be(None)

    }

    scenario("A limit order bid is filled by multiple limit order asks.") {

      val seller1 = TestProbe()
      val seller2 = TestProbe()
      val buyer = TestProbe()

      Given("A large limit order bid and multiple small limit order asks,")

      val bid1 = LimitBidOrder(buyer.ref, testInstrument, 10.5,
        quantity = 2000)

      val ask1 = LimitAskOrder(seller1.ref, testInstrument, 10.4, 800)

      val ask2 = LimitAskOrder(seller2.ref, testInstrument, 10.3, 1200)

      marketRef ! ask1
      marketRef ! ask2
      marketRef ! bid1

      Then("the limit order bid should be filled in two separate trades.")

      val trade1 = PartialFill(seller2.ref, buyer.ref, testInstrument, 10.3, 1200)
      val trade2 = TotalFill(seller1.ref, buyer.ref, testInstrument, 10.4, 800)

      // each seller receives a single message
      seller2.expectMsg[PartialFill](trade1)
      seller1.expectMsg[TotalFill](trade2)

      // seller should get two messages
      buyer.expectMsg[PartialFill](trade1)
      buyer.expectMsg[TotalFill](trade2)

      // both books should now be empty
      market.askOrderBook.headOption should be(None)
      market.bidOrderBook.headOption should be(None)

    }

    scenario("Partial matching of a large limit order bid.") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("a limit order ask is already on the book")

      val ask1 = LimitAskOrder(seller.ref, testInstrument, 2.8, 100)

      market.askOrderBook += ask1

      When("a relatively large limit order bid is received,")

      val largeBid = LimitBidOrder(buyer.ref, testInstrument, 3.0, 1000)

      marketRef ! largeBid

      Then("the limit order bid should only be partially filled.")

      val trade = PartialFill(seller.ref, buyer.ref, testInstrument, 2.8, 100)

      // participants should be informed
      seller.expectMsg(trade)
      buyer.expectMsg(trade)

      // ask book should be empty
      market.askOrderBook.headOption should be(None)

      // bid book should contain a residual bid
      val residualBid = largeBid.split(900)
      market.bidOrderBook.head should be(residualBid)

      market.bidOrderBook.clear()
    }

    scenario("Partial matching of a large limit order ask.") {

      val seller = TestProbe()
      val buyer = TestProbe()

      Given("a limit order bid is already on the book")
      val bid1 = LimitBidOrder(buyer.ref, testInstrument, 2.8, 100)

      market.bidOrderBook += bid1

      When("a relatively large limit order ask is received,")
      val largeAsk = LimitAskOrder(seller.ref, testInstrument, 2.5, 500)

      marketRef ! largeAsk

      Then("the limit order ask should only be partially filled.")

      val trade = PartialFill(seller.ref, buyer.ref, testInstrument, 2.8, 100)

      // participants should be informed
      seller.expectMsg(trade)
      buyer.expectMsg(trade)

      // ask book should contain a residual ask
      val residualAsk = largeAsk.split(400)
      market.askOrderBook.head should be(residualAsk)
      market.askOrderBook.clear()

      // bid book should be empty
      market.bidOrderBook.headOption should be(None)
    }
  }

  scenario("Matching of a small limit order ask.") {

    val seller = TestProbe()
    val buyer = TestProbe()

    Given("a relatively large limit order bid is already on the book")
    val bid1 = LimitBidOrder(buyer.ref, testInstrument, 3.8, 350)

    market.bidOrderBook += bid1

    When("a small limit order ask is received,")
    val smallAsk = LimitAskOrder(seller.ref, testInstrument, 2.95, 10)

    marketRef ! smallAsk

    Then("the limit order ask should be filled leaving a residual bid.")

    val trade = PartialFill(seller.ref, buyer.ref, testInstrument, 3.8, 10)

    // participants should be informed
    seller.expectMsg(trade)
    buyer.expectMsg(trade)

    // bid book should contain a residual ask
    val residualBid = bid1.split(340)
    market.bidOrderBook.head should be(residualBid)
    market.bidOrderBook.clear()

    // ask book should be empty
    market.askOrderBook.headOption should be(None)
  }

  scenario("Matching of a small limit order bid.") {

    val seller = TestProbe()
    val buyer = TestProbe()

    Given("a relatively large limit order ask is already on the book")
    val ask1 = LimitAskOrder(seller.ref, testInstrument, 36.8, 450)

    market.askOrderBook += ask1

    When("a small limit order bid is received,")

    val smallBid = LimitBidOrder(buyer.ref, testInstrument, 40.95, 100)
    marketRef ! smallBid

    Then("the limit order bid should be filled leaving a residual ask.")

    val trade = PartialFill(seller.ref, buyer.ref, testInstrument, 36.8, 100)

    // participants should be informed
    seller.expectMsg(trade)
    buyer.expectMsg(trade)

    // ask book should contain a residual ask
    val residualAsk = ask1.split(350)
    market.askOrderBook.head should be(residualAsk)
    market.askOrderBook.clear()

    // bid book should be empty
    market.bidOrderBook.headOption should be(None)
  }
}