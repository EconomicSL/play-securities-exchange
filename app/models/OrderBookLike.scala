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
sealed trait OrderBookLike {

  def instrument: String

  def bestLimitOrder: Option[LimitPriceLike]

}


case class AskOrderBook(instrument: String) extends
  mutable.PriorityQueue[AskOrderLike] with
  OrderBookLike {

  def bestLimitOrder: Option[LimitAskOrder] = {
    val bestLimitAskOrder = this find(_.isInstanceOf[LimitAskOrder])

    bestLimitAskOrder match {
      case Some(ask: LimitAskOrder) => Some(ask)
      case None => None
    }
  }

}


case class BidOrderBook(instrument: String) extends
  mutable.PriorityQueue[BidOrderLike] with
  OrderBookLike {

  def bestLimitOrder: Option[LimitBidOrder] = {
    val bestLimitOrderBid = this find(_.isInstanceOf[LimitBidOrder])

    bestLimitOrderBid match {
      case Some(bid: LimitBidOrder) => Some(bid)
      case None => None
    }
  }

}
