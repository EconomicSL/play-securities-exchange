import akka.actor.ActorSystem
import akka.testkit.{TestProbe, TestActorRef, TestKit}
import models.{InstrumentTicker, UnwatchInstrument, WatchInstrument, Tick}
import org.scalatest.{GivenWhenThen, Matchers, FeatureSpecLike}


class InstrumentTickerSpec extends
  TestKit(ActorSystem("TestSystem")) with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers {

  /** Shutdown TestSystem after running tests. */
  def afterAll(): Unit = {
    system.shutdown()
  }

  feature("InstrumentTicker should be able to add actors as watchers.") {

    val tickerRef = TestActorRef(new InstrumentTicker("GOOG"))

    val ticker = tickerRef.underlyingActor

    scenario("Some actor requests to be added as a watcher") {

      Given("that the ticker's set of watchers is empty")

      When("a WatchInstrument message is received from some actor")

        tickerRef.tell( WatchInstrument, testActor )

      Then("that actor should be added to the set of watchers")

        assert(ticker.watchers.contains(testActor))

    }

  }

  feature("InstrumentTicker should be able to remove actors as watchers.") {

    val tickerRef = TestActorRef(new models.InstrumentTicker("GOOG"))

    val ticker = tickerRef.underlyingActor

    scenario("Some actor requests to be removed as a watcher") {

      Given("that the ticker's set of watchers contains an actor")

      ticker.watchers += testActor

      When("an UnwatchInstrument message is received from that actor")

      tickerRef.tell(UnwatchInstrument, testActor)

      Then("that actor should be removed from the set of watchers")

      ticker.watchers.contains(testActor) should be(false)

    }

  }

  feature("InstrumentTicker should be able to receive and store security ticks.") {

    val tickerRef = TestActorRef(new models.InstrumentTicker("GOOG"))

    val ticker = tickerRef.underlyingActor

    scenario("A ticker with no watchers receives a tick.") {

      val tick = Tick("GOOG", 1.5)

      Given("that the ticker has no watchers")

      assert(ticker.watchers.isEmpty)

      Given("that the ticker has no history")

      assert(ticker.history.isEmpty)

      When("a ticker receives a tick")

      tickerRef ! tick

      Then("that tick should be added to its history.")

      ticker.history.head should be(tick)

    }

  }

  feature("InstrumentTicker should notify all watchers after receiving a tick.") {

    val tickerRef = TestActorRef(new models.InstrumentTicker("GOOG"))

    val ticker = tickerRef.underlyingActor

    scenario("A ticker with several watchers receives a tick.") {

      val tick = Tick("GOOG", 1.5)

      Given("that the ticker has several watchers")
      val watcher1 = TestProbe()
      val watcher2 = TestProbe()

      ticker.watchers += (watcher1.ref, watcher2.ref)

      Given("that the ticker has no history")

      assert(ticker.history.isEmpty)

      When("a ticker receives a tick")

      tickerRef ! tick

      Then("that the ticker should notify its watchers")

      watcher1.expectMsg[Tick](tick)
      watcher2.expectMsg[Tick](tick)

      Then("the tick should be added to thet ticker's history.")

      ticker.history.head should be(tick)

    }

  }

}
