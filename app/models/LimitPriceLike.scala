package models

/** Trait representing a limit price for a particular security.
  *
  * The LimitPriceLike trait should be mixed-in with both AskOrderLike and BidOrderLike
  * traits to create instance of LimitAskOrder and LimitBidOrder classes.
  */
trait LimitPriceLike {

  def limitPrice: Double
  
}
