/*
Copyright 2015 David R. Pugh

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

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
                         instrument: SecurityLike,
                         limitPrice: Double,
                         quantity: Double) extends
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
  override def split(newQuantity: Double): BidOrderLike = {
    LimitBidOrder(tradingPartyRef, instrument, limitPrice, newQuantity)
  }

  /** String representation of a limit order. */
  override def toString: String = {
    s",${tradingPartyRef.path.name},$getClass,$instrument,$limitPrice,$quantity"
  }

}

