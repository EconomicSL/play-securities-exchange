import akka.actor.ActorSystem
import akka.testkit.{TestProbe, TestActorRef, TestKit}
import models.{SecurityTicker, UnwatchSecurity, WatchSecurity, Tick}
import org.scalatest.{GivenWhenThen, Matchers, FeatureSpecLike}


class SecurityTickerSpec extends
  TestKit(ActorSystem("TestSystem")) with
  FeatureSpecLike with
  GivenWhenThen with
  Matchers {

  /** Shutdown TestSystem after running tests. */
  def afterAll(): Unit = {
    system.shutdown()
  }

  feature("SecurityTicker should be able to add actors as watchers.") {

    val tickerRef = TestActorRef(new SecurityTicker("GOOG"))

    val ticker = tickerRef.underlyingActor

    scenario("Some actor requests to be added as a watcher") {

      Given("that the ticker's set of watchers is empty")

      When("a WatchSecurity message is received from some actor")

        tickerRef.tell( WatchSecurity, testActor )

      Then("that actor should be added to the set of watchers")

        assert(ticker.watchers.contains(testActor))

    }

  }

  feature("SecurityTicker should be able to remove actors as watchers.") {

    val tickerRef = TestActorRef(new models.SecurityTicker("GOOG"))

    val ticker = tickerRef.underlyingActor

    scenario("Some actor requests to be removed as a watcher") {

      Given("that the ticker's set of watchers contains an actor")

      ticker.watchers += testActor

      When("an UnwatchSecurity message is received from that actor")

      tickerRef.tell(UnwatchSecurity, testActor)

      Then("that actor should be removed from the set of watchers")

      ticker.watchers.contains(testActor) should be(false)

    }

  }

  feature("SecurityTicker should be able to receive and store security ticks.") {

    val tickerRef = TestActorRef(new models.SecurityTicker("GOOG"))

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

  feature("SecurityTicker should notify all watchers after receiving a tick.") {

    val tickerRef = TestActorRef(new models.SecurityTicker("GOOG"))

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
