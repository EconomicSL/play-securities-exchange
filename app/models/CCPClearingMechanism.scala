package models

import akka.actor.{Props, ActorLogging, Actor}


/** Central counter party (CCP) clearing mechanism.
  *
  * Central counter party inserts itself as trading party with ask and bid actors.
  * 
  */
class CCPClearingMechanism extends Actor
  with ActorLogging {

  def receive: Receive = {
    case fill: PartialFill =>
      // insert self as counter party to the bid trading party
      val askTransactionHandler = context.actorOf(Props[TransactionHandler])
      askTransactionHandler ! PartialFill(self, fill.bidTradingPartyRef, fill.instrument, fill.price, fill.quantity)

      // insert self as counter party to the ask trading party
      val bidTransactionHandler = context.actorOf(Props[TransactionHandler])
      bidTransactionHandler ! PartialFill(fill.askTradingPartyRef, self, fill.instrument, fill.price, fill.quantity)

    case fill: TotalFill =>
      // insert self as counter party to the bid trading party
      val askTransactionHandler = context.actorOf(Props[TransactionHandler])
      askTransactionHandler ! TotalFill(self, fill.bidTradingPartyRef, fill.instrument, fill.price, fill.quantity)

      // insert self as counter party to the ask trading party
      val bidTransactionHandler = context.actorOf(Props[TransactionHandler])
      bidTransactionHandler ! TotalFill(fill.askTradingPartyRef, self, fill.instrument, fill.price, fill.quantity)
  }
  
}
