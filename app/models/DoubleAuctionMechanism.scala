package models

import akka.actor.{ActorLogging, Actor}
import akka.agent.Agent


/** Implements a Continuous Double Auction (CDA) market.
  *
  * The Continuous Double Auction (CDA) market is a mechanism that matches
  * buyers and sellers of a particular security and determines the prices at
  * and quantities at which orders are filled. At any point in time, traders
  * can place orders in the form of bids (i.e., buy orders) and asks (i.e.,
  * sell orders). Outstanding orders are maintained in order books.
  *
  * @param instrument the security traded on the market.
  *
  * @author davidrpugh
  */
class DoubleAuctionMechanism(val instrument: String) extends Actor with
  ActorLogging with
  MatchingEngineLike {

  val askOrderBook: AskOrderBook = AskOrderBook(instrument)

  val bidOrderBook: BidOrderBook = BidOrderBook(instrument)

  var referencePrice: Option[Double] = None

  /** Receive a bid (i.e. buy) or ask (i.e., sell) order for an instrument. */
  def receive = {
    case askOrder: AskOrderLike => tryFindMatchingBid(askOrder)
    case bidOrder: BidOrderLike => tryFindMatchingAsk(bidOrder)
  }

  /** Attempt to match incoming Bid orders.
    *
    * If the incoming Bid order crosses with the top Ask order, then a trade
    * can be executed; if the incoming Bid order does not cross the top Ask
    * order, then the incoming Bid order is added to the bid order book.
    *
    * @param incoming an incoming Bid order
    */
  def tryFindMatchingAsk(incoming: BidOrderLike): Unit = {
    askOrderBook.headOption match {
      case Some(topAsk) =>
        if (incoming.crosses(topAsk)) {
          matchWithTopAsk(incoming)
        } else {
          bidOrderBook += incoming
        }
      case None => bidOrderBook += incoming  // ask book might be empty!
    }
  }

  /** Attempt to match incoming Ask orders.
    *
    * If the incoming Ask order crosses with the top Bid order, then a trade
    * can be executed; if the incoming Ask order does not cross the top Bid
    * order, then the incoming Ask order is added to the ask order book.
    *
    * @param incoming an incoming Ask order
    */
  def tryFindMatchingBid(incoming: AskOrderLike): Unit = {
    bidOrderBook.headOption match {
      case Some(topBid) =>
        if (incoming.crosses(topBid)) {
          matchWithTopBid(incoming)
        } else {
          askOrderBook += incoming
        }
      case None => askOrderBook += incoming  // bid book might be empty!
    }
  }

  /** Match an incoming Ask order with the top existing Bid order.
    *
    * @param incoming an incoming Ask order
    */
  def matchWithTopBid(incoming: AskOrderLike): Unit = {

    val topBid = bidOrderBook.dequeue()  // remove top bid from the queue!
    val price = incoming.formPrice(topBid)
    val excessDemand = topBid.quantity - incoming.quantity

    if (excessDemand > 0) {
      // need to generate a residual Bid
      val residualBid = topBid.split(excessDemand)
      generatePartialFill(incoming, topBid, price, incoming.quantity)
      tryFindMatchingAsk(residualBid)  // recurse!
    } else if (excessDemand < 0) {
      // need to generate a residual Ask
      val residualAsk = incoming.split(-excessDemand)
      generatePartialFill(incoming, topBid, price, topBid.quantity)
      tryFindMatchingBid(residualAsk)  // recurse!
    } else {
      // desired quantities match exactly!
      generateTotalFill(incoming, topBid, price, topBid.quantity)
    }
  }

  /** Match an incoming Bid order with the top existing Ask order.
    *
    * @param incoming an incoming Bid order
    */
  def matchWithTopAsk(incoming: BidOrderLike): Unit = {

    val topAsk = askOrderBook.dequeue() // remove top ask from the queue!
    val price = incoming.formPrice(topAsk)
    val excessDemand = incoming.quantity - topAsk.quantity

    if (excessDemand > 0) {
      // need to generate a residual Bid
      val residualBid = incoming.split(excessDemand)
      generatePartialFill(topAsk, incoming, price, topAsk.quantity)
      tryFindMatchingAsk(residualBid)  // recurse!
    } else if (excessDemand < 0) {
      // need to generate a residual Ask
      val residualAsk = topAsk.split(-excessDemand)
      generatePartialFill(topAsk, incoming, price, incoming.quantity)
      tryFindMatchingBid(residualAsk)  // recurse!
    } else {
      // desired quantities match exactly!
      generateTotalFill(topAsk, incoming, price, incoming.quantity)
    }
  }

  /** Generate a partially filled order.
    *
    * @param ask an Ask order
    * @param bid a Bid order
    * @param price price at which order is filled.
    * @param quantity quantity involved in the filled order.
    */
  def generatePartialFill(ask: AskOrderLike, bid: BidOrderLike, price: Double, quantity: Int): Unit = {
    val partialFill = PartialFill(ask.tradingPartyRef, bid.tradingPartyRef, instrument, price, quantity)
    log.info(s",${System.nanoTime()}" + partialFill.toString)
    informParticipants(partialFill)
    updateReferencePrice(price)
  }

  /** Generate a totally filled order.
    *
    * @param ask an Ask order
    * @param bid a Bid order
    * @param price price at which order is filled.
    * @param quantity quantity involved in the filled order.
    */
  def generateTotalFill(ask: AskOrderLike, bid: BidOrderLike, price: Double, quantity: Int): Unit = {
    val totalFill = TotalFill(ask.tradingPartyRef, bid.tradingPartyRef, instrument, price, quantity)
    log.info(s",${System.nanoTime()}" + totalFill.toString)
    informParticipants(totalFill)
    updateReferencePrice(price)
  }

  /** Inform participants of the filled order.
    *
    * @param order a partially or totally filled order
    */
  def informParticipants(order: FillLike): Unit ={
    order.askTradingPartyRef ! order
    order.bidTradingPartyRef ! order
  }

  /** Update the reference price for the security. */
  def updateReferencePrice(price: Double): Unit = {
    referencePrice = Some(price)
  }

}












