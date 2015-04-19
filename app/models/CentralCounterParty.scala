package models

import akka.actor.{ActorLogging, Actor}


/** Central counter party clearing mechanism. */
class CentralCounterParty extends Actor
  with ActorLogging {

  def receive: Receive = {
    case fill: FillLike => ???
  }

}
