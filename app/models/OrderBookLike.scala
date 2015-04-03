package models

import scala.collection.mutable

/** Basic representation of an order book.
  *
  * The order book trait is designed to be mixed in with AskBook and BidBook.
  *
  */
sealed trait OrderBookLike {

  def instrument: String

  def bestLimitOrder: Option[LimitOrderLike]

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
