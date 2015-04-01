package models


import akka.actor.{ActorRef, Actor}

import scala.collection.immutable.HashSet
import scala.collection.mutable

/** Class representing a security ticker.
  *
  * There is exactly one SecurityTicker per security symbol. Each SecurityTicker
  * maintains a list of other actors watching the security as well as its own
  * ticker history.
  *
  * @param symbol a security ticker symbol
  */
class SecurityTicker(symbol: String) extends Actor {

  /** Each ticker maintains a list of participants watching the security. */
  protected[this] var watchers: HashSet[ActorRef] = HashSet.empty[ActorRef]

  /** Each ticker stores its own history. */
  var history: mutable.Queue[Tick] = mutable.Queue.empty[Tick]

  def receive: Receive = {
    case tick: Tick =>
      history += tick;  // add the tick to the ticker history
      watchers.foreach(_ ! tick)  // notify all watchers
    case WatchSecurity =>
      watchers += sender()  // add sender to the set of watchers
    case UnwatchSecurity =>
      watchers -= sender()  // remove sender from the set of watchers
  }

}


case class Tick(symbol: String, price: Double)

case object WatchSecurity

case object UnwatchSecurity