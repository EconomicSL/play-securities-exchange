package models

import akka.actor.ActorRef


/** Represents a limit ask order.
  *
  * A LimitAskOrder represents an order to sell a certain quantity of shares of a
  * specific security at prices greater than or equal to some limit price.
  *
  * @param tradingPartyRef ActorRef for the trading party submitting the order.
  * @param instrument Security for which the order is being placed.
  * @param price Limit price for the order.
  * @param quantity Desired quantity of the security.
  */
case class LimitBidOrder(tradingPartyRef: ActorRef,
                         instrument: String,
                         price: Double,
                         quantity: Int) extends
  BidOrderLike with
  PricedOrderLike {

  /** Crossing logic for a limit bid order.
    *
    * @param ask some ask order.
    * @return true if the limit order ask cross the bid; false otherwise.
    */
  def crosses(ask: AskOrderLike): Boolean = ask match {
    case ask: PricedOrderLike => price >= ask.price
  }

  /** Price formation rules for a limit bid order.
    *
    * @param ask some ask order.
    * @return the trade price between a limit bid order and some ask order.
    */
  def formPrice(ask: AskOrderLike): Double = ask match {
    case ask: PricedOrderLike => ask.price
  }

  /** Split a limit ask order
    *
    * @param newQuantity Desired quantity for the new order.
    * @return new limit order ask.
    */
  def split(newQuantity: Int): OrderLike = {
    LimitBidOrder(tradingPartyRef, instrument, price, newQuantity)
  }

  /** String representation of a limit order. */
  override def toString: String = {
    s",${tradingPartyRef.path.name},$getClass,$instrument,$price,$quantity"
  }

}

