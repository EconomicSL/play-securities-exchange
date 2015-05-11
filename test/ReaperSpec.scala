import akka.actor.{ActorSystem, Props, ActorRef}
import akka.testkit.{TestKit, ImplicitSender, TestProbe}
import models.Reaper
import org.scalatest._
import org.scalatest.matchers.MustMatchers


// Our test reaper.  Sends the snooper a message when all souls have been reaped
class TestReaper(snooper: ActorRef) extends Reaper {
  def allSoulsReaped(): Unit = snooper ! "Dead"
}

class ReaperSpec extends TestKit(ActorSystem("ReaperSpec"))
  with ImplicitSender
  with FeatureSpecLike
  with GivenWhenThen
  with Matchers
  with BeforeAndAfterAll {

  import Reaper._

  override def afterAll() {
    system.shutdown()
  }

  feature("Reaper should be able to shutdown the actor system.") {

    scenario("Reaper is watching some dummy actors.") {

      Given("Some dummy actors, some of which are watched by the Reaper,")
      // Set up some dummy Actors
      val a = TestProbe()
      val b = TestProbe()
      val c = TestProbe()
      val d = TestProbe()

      // Build our reaper
      val reaper = system.actorOf(Props(new TestReaper(testActor)))

      // Watch a couple
      reaper ! WatchMe(a.ref)
      reaper ! WatchMe(d.ref)

      When("the dummy actors being watched have all been terminated,")
      // Stop them
      system.stop(a.ref)
      system.stop(d.ref)

      Then("the Reaper should get called!")

      // Make sure we've been called
      expectMsg("Dead")

    }
  }
}