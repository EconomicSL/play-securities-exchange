package models

import akka.actor.Actor


/** CashHolder trait
  *
  * CashHolder trait should be mixed in with any Actor that needs to process
  * cash transactions.
  */
trait CashHolder {
  this: Actor =>

  /* Actor's cash holdings. */
  var cash: Double

  /* Decrements an actor's cash holdings. */
  def dishoardCash(amount: Double): Unit = {
    cash -= amount
  }

  /* Increments an actor's cash holdings. */
  def hoardCash(amount: Double): Unit = {
    cash += amount
  }

  def handleCashTransaction: Receive = {
    case Payment(amount) =>
      hoardCash(amount)
    case RequestPayment(amount) =>
      dishoardCash(amount)
      sender() ! Payment(amount)
  }

}


case class Payment(amount: Double) {

  require(amount > 0.0)

}


case class RequestPayment(amount: Double) {

  require(amount > 0.0)

}


