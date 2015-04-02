package models


trait LimitOrderLike extends OrderLike {

  def price: Double

  /** String representation of a limit order. */
  override def toString: String = {
    s",${tradingPartyRef.path.name},$getClass,$instrument,$price,$quantity"
  }

}
