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

import scala.collection.mutable


/** Basic representation of an order book.
  *
  * The order book trait is designed to be mixed in with AskBook and BidBook.
  *
  */
sealed trait OrderBookLike[T <: OrderLike] {

  /** The best limit order currently in the book. */
  def bestLimitOrder: Option[LimitPriceLike]

  /** The order bok. */
  def orderBook: mutable.PriorityQueue[T]

  def tradable: AssetLike

}


case class AskOrderBook(tradable: SecurityLike) extends OrderBookLike[AskOrderLike] {

  def bestLimitOrder: Option[LimitAskOrder] = {
    val bestLimitAskOrder = orderBook.find(order => order.isInstanceOf[LimitAskOrder])
    bestLimitAskOrder.asInstanceOf[Option[LimitAskOrder]]
  }

  val orderBook = mutable.PriorityQueue[AskOrderLike]()

}


case class BidOrderBook(tradable: SecurityLike) extends OrderBookLike[BidOrderLike] {

  def bestLimitOrder: Option[LimitBidOrder] = {
    val bestLimitBidOrder = orderBook.find(order => order.isInstanceOf[LimitBidOrder])
    bestLimitBidOrder.asInstanceOf[Option[LimitBidOrder]]
  }

  val orderBook = mutable.PriorityQueue[BidOrderLike]()

}
