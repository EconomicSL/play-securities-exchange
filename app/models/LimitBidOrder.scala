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
  LimitOrderLike with
  BidOrderLike {

  /** Split a limit ask order
    *
    * @param newQuantity Desired quantity for the new order.
    * @return new limit order ask.
    */
  def split(newQuantity: Int): OrderLike = {
    LimitBidOrder(tradingPartyRef, instrument, price, newQuantity)
  }

}

