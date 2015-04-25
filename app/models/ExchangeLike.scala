package models

import akka.actor.{ActorRef, ActorLogging, Actor}


trait ExchangeLike extends Actor
  with ActorLogging {

  def clearingMechanism: ActorRef

}
