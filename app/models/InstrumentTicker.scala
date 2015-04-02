package models


import akka.actor.{ActorRef, Actor}

import scala.collection.immutable.HashSet
import scala.collection.mutable


/** Class representing an instrument ticker.
  *
  * There is exactly one InstrumentTicker per instrument symbol. Each SecurityTicker
  * maintains a list of other actors watching the instrument as well as its own
  * ticker history.
  *
  * @param symbol an instrument ticker symbol
  */
class InstrumentTicker(symbol: String) extends Actor {

  /** Each ticker maintains a list of participants watching the instrument. */
  var watchers: HashSet[ActorRef] = HashSet.empty[ActorRef]

  /** Each ticker stores its own history. */
  var history: mutable.Queue[Tick] = mutable.Queue.empty[Tick]

  def receive: Receive = {
    case tick: Tick =>
      history += tick;  // add the tick to the ticker history
      watchers.foreach(_ ! tick)  // notify all watchers
    case WatchInstrument =>
      watchers += sender()  // add sender to the set of watchers
    case UnwatchInstrument =>
      watchers -= sender()  // remove sender from the set of watchers
  }

}


case class Tick(symbol: String, price: Double)

case object WatchInstrument

case object UnwatchInstrument