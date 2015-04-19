package models

import akka.actor.{ActorRef, Actor}


/** Handles clearing of an individual transaction.
  *
  * Reduced form model of a payments system.
  *
  */
class TransactionHandler extends Actor {

  var buyer: ActorRef = Actor.noSender

  var seller: ActorRef = Actor.noSender

  var payment: Option[Payment] = None

  var securities: Option[Securities] = None

  def checkIfComplete(): Unit = {

    if (payment.isDefined && securities.isDefined) {
      buyer ! securities.get
      seller ! payment.get
      context.stop(self)
    }

  }

  def receive: Receive = {
    case fill: FillLike =>
      seller = fill.askTradingPartyRef
      seller ! RequestSecurities(fill.instrument, fill.quantity)
      buyer = fill.bidTradingPartyRef
      buyer ! RequestPayment(fill.price * fill.quantity)
    case Payment(amount) =>
      payment = Some(Payment(amount)); checkIfComplete()
    case Securities(instrument, quantity) =>
      securities = Some(Securities(instrument, quantity)); checkIfComplete()
  }


}


case class RequestPayment(amount: Double) {

  require(amount > 0.0)

}

case class RequestSecurities(instrument: String, quantity: Int) {

  require(quantity > 0)

}

case class Payment(amount: Double) {

  require(amount > 0.0)

}

case class Securities(instrument: String, quantity: Int) {

  require(quantity > 0)

}


