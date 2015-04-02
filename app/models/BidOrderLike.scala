package models

/** Trait representing an Bid order.
  *
  * A Bid order is an order to buy a security. The BidOrderLike trait should
  * be mixed in with each specific type of order (i.e., limit orders, market
  * orders, etc).
  *
  */
trait BidOrderLike {

  val buy = true

}
