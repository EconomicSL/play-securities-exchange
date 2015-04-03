package models


trait MatchingEngineLike {

  def askOrderBook: AskOrderBook

  def bidOrderBook: BidOrderBook

  def instrument: String

  def referencePrice: Option[Double]

}

