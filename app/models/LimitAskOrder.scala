package models

import akka.actor.{Actor, ActorRef}

import scala.concurrent.duration.FiniteDuration



/** Represents a limit ask order.
  *
  * A LimitAskOrder represents an order to sell a certain quantity of shares of a
  * specific security at prices greater than or equal to some limit price.
  *
  * @param tradingPartyRef ActorRef for the trading party submitting the order.
  * @param instrument Security for which the order is being placed.
  * @param timeInForce Duration for which the order is in force.
  * @param price Limit price for the order.
  * @param quantity Desired quantity of the security.
  */
case class LimitAskOrder(tradingPartyRef: ActorRef,
                         instrument: String,
                         timeInForce: Option[FiniteDuration],
                         price: Double,
                         quantity: Int) extends Actor with
  OrderLike with
  AskOrderLike with
  LimitOrderLike {

  /** If necessary, schedule the order cancellation event. */
  override def preStart(): Unit = {
    timeInForce match {
      case Some(duration) => context.system.scheduler.scheduleOnce(duration, self, CancelOrder)
      case None => // Do nothing!
    }
  }

  def receive: Receive = {
    case CancelOrder =>
      context.stop(self)  // If order is cancelled, stop the Actor
  }

  /** Split a limit ask order
    *
    * @param newQuantity Desired quantity for the new order.
    * @return new limit order ask.
    */
  def split(newQuantity: Int): AskOrderLike = {
    LimitAskOrder(tradingPartyRef, instrument, timeInForce, price, newQuantity)
  }

  /** String representation of a limit ask order. */
  override def toString: String = {
    s",${tradingPartyRef.path.name},$getClass,$instrument,$price,$quantity"
  }

}
