package models

import akka.actor.{ActorRef, Actor}


/** Handles clearing of an individual transaction.
  *
  * Reduced form model of a payments system.
  *
  * @todo handle failure to send payment and failure to send securities.
  *
  */
class TransactionHandler extends Actor {

  var buyer: ActorRef = Actor.noSender

  var seller: ActorRef = Actor.noSender

  var payment: Option[Payment] = None

  var securities: Option[Securities] = None

  def paymentReceived: Boolean = {
    payment.isDefined
  }

  def securitiesReceived: Boolean = {
    securities.isDefined
  }

  def receive: Receive = {
    case fill: FillLike =>
      seller = fill.askTradingPartyRef; buyer = fill.bidTradingPartyRef
      seller ! RequestSecurities(fill.instrument, fill.quantity)
      buyer ! RequestPayment(fill.price * fill.quantity)
    case Payment(amount) =>
      payment = Some(Payment(amount))
      if (securitiesReceived) {
        buyer ! securities.get
        seller ! payment.get
        context.stop(self)
      }
    case Securities(instrument, quantity) =>
      securities = Some(Securities(instrument, quantity))
      if (paymentReceived) {
        buyer ! securities.get
        seller ! payment.get
        context.stop(self)
      }
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


