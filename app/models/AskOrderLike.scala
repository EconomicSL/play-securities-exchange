package models

/** Trait representing an Ask order.
  *
  * An Ask order is an order to sell a security. The AskOrderLike trait should
  * be mixed in with each specific type of order (i.e., limit orders, market
  * orders, etc).
  *
  */
trait AskOrderLike {

  val buy = false

}
