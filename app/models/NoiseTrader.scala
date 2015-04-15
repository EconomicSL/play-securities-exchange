package models

import akka.actor.{ActorRef, Actor}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable
import scala.util.Random


case class NoiseTrader(assets: mutable.Map[String, Int],
                       cash: Double,
                       market: ActorRef,
                       prng: Random) extends Actor with
  TraderLike {

  val conf = ConfigFactory.load("traders.conf")

  val maxPrice: Double = conf.getDouble("maxPrice")
  
  val maxQuantity: Int = conf.getInt("maxQuantity")

  val askOrderProbability: Double = conf.getDouble("askOrderProbability")

  val tickerLength: Int = conf.getInt("tickerLength") 

  def decideAskPrice(): Double = {
    prng.nextDouble() * maxPrice
  }

  def decideBidPrice(): Double = {
    prng.nextDouble() * maxPrice
  }

  def decideAskQuantity(): Int = {
    prng.nextInt() * maxQuantity
  }

  def decideBidQuantity(): Int = {
    prng.nextInt() * maxQuantity
  }

  def decideInstrument(): String = {
    prng.nextString(tickerLength)
  }

  def generateNewAskOrder(): AskOrderLike = {
    LimitAskOrder(self, decideInstrument(), decideAskPrice(), decideAskQuantity())
  }

  def generateNewBidOrder(): BidOrderLike = {
    LimitBidOrder(self, decideInstrument(), decideBidPrice(), decideBidQuantity())
  }

  def generateNewOrder(): OrderLike = {
    if (prng.nextDouble() < askOrderProbability) {
      generateNewAskOrder()
    } else {
      generateNewBidOrder()
    }
  }

  def receive: Receive = {
    case StartTrading =>
      market ! generateNewOrder()
    case OrderReceived =>
      market ! generateNewOrder()
  }

}


case object StartTrading
