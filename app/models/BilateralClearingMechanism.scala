package models

import akka.actor.{Props, ActorLogging, Actor}


/** Bilateral clearing mechanism. */
class BilateralClearingMechanism extends Actor
  with ActorLogging {

  def receive: Receive = {
    case fill: FillLike =>
      val transactionHandler = context.actorOf(Props[TransactionHandler])
      transactionHandler ! fill
  }

}
