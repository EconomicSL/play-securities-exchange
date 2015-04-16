package models

import akka.actor.ActorRef


/** Represents a limit ask order.
  *
  * A LimitAskOrder represents an order to sell a certain quantity of shares of a
  * specific security at prices greater than or equal to some limit price.
  *
  * @param tradingPartyRef ActorRef for the trading party submitting the order.
  * @param instrument Security for which the order is being placed.
  * @param limitPrice Limit price for the order.
  * @param quantity Desired quantity of the security.
  */
case class LimitBidOrder(tradingPartyRef: ActorRef,
                         instrument: String,
                         limitPrice: Double,
                         quantity: Int) extends
  BidOrderLike with
  LimitPriceLike {

  require(limitPrice > 0, "Price must be strictly positive.")

  require(quantity > 0, "Quantity must be strictly positive.")

  /** Crossing logic for a limit bid order.
    *
    * @param ask some ask order.
    * @return true if the limit order ask cross the bid; false otherwise.
    */
  def crosses(ask: AskOrderLike): Boolean = ask match {
    case ask: LimitPriceLike => limitPrice >= ask.limitPrice
  }

  /** Price formation rules for a limit bid order.
    *
    * @param ask some ask order.
    * @return the trade price between a limit bid order and some ask order.
    */
  def formPrice(ask: AskOrderLike): Double = ask match {
    case ask: LimitPriceLike => ask.limitPrice
  }

  /** Split a limit ask order
    *
    * @param newQuantity Desired quantity for the new order.
    * @return new limit order ask.
    */
  override def split(newQuantity: Int): BidOrderLike = {
    LimitBidOrder(tradingPartyRef, instrument, limitPrice, newQuantity)
  }

  /** String representation of a limit order. */
  override def toString: String = {
    s",${tradingPartyRef.path.name},$getClass,$instrument,$limitPrice,$quantity"
  }

}

