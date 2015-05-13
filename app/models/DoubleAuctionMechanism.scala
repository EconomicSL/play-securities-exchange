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

import akka.actor.{Actor, ActorLogging, Props}


object DoubleAuctionMechanism {

  def props(instrument: SecurityLike): Props = {
    Props(DoubleAuctionMechanism(instrument))
  }

}


case class DoubleAuctionMechanism(tradable: SecurityLike) extends Actor
  with ActorLogging
  with MatchingEngineLike {

  val askOrders: AskOrderBook = AskOrderBook(tradable)

  val bidOrders: BidOrderBook = BidOrderBook(tradable)

  var referencePrice: Option[Double] = None

  /** Receive a bid (i.e. buy) or ask (i.e., sell) order for an instrument. */
  def receive = {
    case askOrder: AskOrderLike =>
      tryFindMatchingBid(askOrder)
    case bidOrder: BidOrderLike =>
      tryFindMatchingAsk(bidOrder)
  }

  /** Attempt to match incoming Bid orders.
    *
    * If the incoming Bid order crosses with the top Ask order, then a trade
    * can be executed; if the incoming Bid order does not cross the top Ask
    * order, then the incoming Bid order is added to the bid order book.
    *
    * @param incoming an incoming Bid order
    */
  def tryFindMatchingAsk(incoming: BidOrderLike): Unit = {
    askOrders.orderBook.headOption match {
      case Some(topAsk) =>
        if (incoming.crosses(topAsk)) {
          matchWithTopAsk(incoming)
        } else {
          bidOrders.orderBook += incoming
        }
      case None => bidOrders.orderBook += incoming  // ask book might be empty!
    }
  }

  /** Attempt to match incoming Ask orders.
    *
    * If the incoming Ask order crosses with the top Bid order, then a trade
    * can be executed; if the incoming Ask order does not cross the top Bid
    * order, then the incoming Ask order is added to the ask order book.
    *
    * @param incoming an incoming Ask order
    */
  def tryFindMatchingBid(incoming: AskOrderLike): Unit = {
    bidOrders.orderBook.headOption match {
      case Some(topBid) =>
        if (incoming.crosses(topBid)) {
          matchWithTopBid(incoming)
        } else {
          askOrders.orderBook += incoming
        }
      case None => askOrders.orderBook += incoming  // bid book might be empty!
    }
  }

  /** Match an incoming Ask order with the top existing Bid order.
    *
    * @param incoming an incoming Ask order
    */
  def matchWithTopBid(incoming: AskOrderLike): Unit = {

    val topBid = bidOrders.orderBook.dequeue()  // remove top bid from the queue!
    val price = incoming.formPrice(topBid)
    val excessDemand = topBid.quantity - incoming.quantity

    if (excessDemand > 0) {
      // need to generate a residual Bid
      val residualBid = topBid.split(excessDemand)
      generatePartialFill(incoming, topBid, price, incoming.quantity)
      tryFindMatchingAsk(residualBid)  // recurse!
    } else if (excessDemand < 0) {
      // need to generate a residual Ask
      val residualAsk = incoming.split(-excessDemand)
      generatePartialFill(incoming, topBid, price, topBid.quantity)
      tryFindMatchingBid(residualAsk)  // recurse!
    } else {
      // desired quantities match exactly!
      generateTotalFill(incoming, topBid, price, topBid.quantity)
    }
  }

  /** Match an incoming Bid order with the top existing Ask order.
    *
    * @param incoming an incoming Bid order
    */
  def matchWithTopAsk(incoming: BidOrderLike): Unit = {

    val topAsk = askOrders.orderBook.dequeue() // remove top ask from the queue!
    val price = incoming.formPrice(topAsk)
    val excessDemand = incoming.quantity - topAsk.quantity

    if (excessDemand > 0) {
      // need to generate a residual Bid
      val residualBid = incoming.split(excessDemand)
      generatePartialFill(topAsk, incoming, price, topAsk.quantity)
      tryFindMatchingAsk(residualBid)  // recurse!
    } else if (excessDemand < 0) {
      // need to generate a residual Ask
      val residualAsk = topAsk.split(-excessDemand)
      generatePartialFill(topAsk, incoming, price, incoming.quantity)
      tryFindMatchingBid(residualAsk)  // recurse!
    } else {
      // desired quantities match exactly!
      generateTotalFill(topAsk, incoming, price, incoming.quantity)
    }
  }

  /** Generate a partially filled order.
    *
    * @param ask an Ask order
    * @param bid a Bid order
    * @param price price at which order is filled.
    * @param quantity quantity involved in the filled order.
    */
  def generatePartialFill(ask: AskOrderLike, bid: BidOrderLike, price: Double, quantity: Double): Unit = {
    val partialFill = PartialFilledOrder(ask.id, ask.tradingPartyRef, bid.id, bid.tradingPartyRef, tradable, price, quantity)
    updateReferencePrice(price)
    context.parent ! partialFill
  }

  /** Generate a totally filled order.
    *
    * @param ask an Ask order
    * @param bid a Bid order
    * @param price price at which order is filled.
    * @param quantity quantity involved in the filled order.
    */
  def generateTotalFill(ask: AskOrderLike, bid: BidOrderLike, price: Double, quantity: Double): Unit = {
    val totalFill = TotalFilledOrder(ask.id, ask.tradingPartyRef, bid.id, bid.tradingPartyRef, tradable, price, quantity)
    updateReferencePrice(price)
    context.parent ! totalFill
  }

  /** Update the reference price for the security. */
  def updateReferencePrice(price: Double): Unit = {
    referencePrice = Some(price)
  }

}










