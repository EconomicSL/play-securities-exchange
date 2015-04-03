package models

/** Trait representing an Ask order.
  *
  * An Ask order is an order to sell a security. The AskOrderLike trait should
  * be mixed in with each specific type of order (i.e., limit orders, market
  * orders, etc).
  *
  */
trait AskOrderLike extends OrderLike {

  val buy = false

  /** Whether or not the ask order crosses some bid order. */
  def crosses(other: BidOrderLike): Boolean

  /** Price formation rules. */
  def formPrice(other: BidOrderLike): Double

  /** AskOrders will often need to be split during the matching process. */
  def split(newQuantity: Int): AskOrderLike


}


object AskOrderLike {

  implicit val ordering: Ordering[AskOrderLike] = Ordering.fromLessThan {
    case (existing: LimitAskOrder, incoming: LimitAskOrder) =>
      incoming.limitPrice < existing.limitPrice  // LimitAskOrder with lower limit price get priority
  }

}
