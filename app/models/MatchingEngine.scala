package models


/** Base trait for all matching engines. */
trait MatchingEngine {

  val instrument: String

  val askOrderBook: AskOrderBook

  val bidOrderBook: BidOrderBook

  var referencePrice: Option[Double] = None

  /** Implements crossing logic for various types of orders. */
  def crosses(incoming: OrderLike, top: OrderLike): Boolean

  /** Implements price formation rules for various types of orders. */
  def formPrice(incoming: OrderLike, top: OrderLike): Double

}

