package models

/** Trait representing a limit order for a particular security.
  *
  * The LimitOrderLike trait should be mixed-in with both AskOrderLike and BidOrderLike
  * traits to create instance of LimitAskOrder and LimitBidOrder classes.
  */
trait LimitOrderLike extends OrderLike {

  def price: Double

  /** String representation of a limit order. */
  override def toString: String = {
    s",${tradingPartyRef.path.name},$getClass,$instrument,$price,$quantity"
  }

}
