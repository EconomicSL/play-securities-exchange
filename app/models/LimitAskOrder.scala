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
case class LimitAskOrder(tradingPartyRef: ActorRef,
                         instrument: AssetLike,
                         limitPrice: Double,
                         quantity: Int) extends
  AskOrderLike with
  LimitPriceLike {

  require(limitPrice > 0, "Price must be strictly positive.")

  require(quantity > 0, "Quantity must be strictly positive.")

  /** Crossing logic for a limit ask order.
    *
    * @param bid some bid order.
    * @return true if the limit order ask cross the bid; false otherwise.
    */
  def crosses(bid: BidOrderLike): Boolean = bid match {
    case bid: LimitPriceLike => limitPrice <= bid.limitPrice
  }

  /** Price formation rules for limit ask orders.
    *
    * @param bid some bid order.
    * @return the trade price between a limit ask order and some bid order.
    */
  def formPrice(bid: BidOrderLike): Double = bid match {
    case bid: LimitPriceLike => bid.limitPrice
  }

  /** Split a limit ask order
    *
    * @param newQuantity Desired quantity for the new order.
    * @return new limit order ask.
    */
  def split(newQuantity: Int): AskOrderLike = {
    LimitAskOrder(tradingPartyRef, instrument, limitPrice, newQuantity)
  }

  /** String representation of a limit order. */
  override def toString: String = {
    s",${tradingPartyRef.path.name},$getClass,$instrument,$limitPrice,$quantity"
  }

}

