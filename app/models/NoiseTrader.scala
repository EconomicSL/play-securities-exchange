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

  private val conf = ConfigFactory.load("traders.conf")

  private val maxPrice: Double = conf.getDouble("maxPrice")
  
  private val maxQuantity: Int = conf.getInt("maxQuantity")

  private val askOrderProbability: Double = conf.getDouble("askOrderProbability")

  def decideAskPrice(): Double = {
    prng.nextDouble() * maxPrice
  }

  def decideBidPrice(): Double = {
    prng.nextDouble() * maxPrice
  }

  def decideAskQuantity(): Int = {
    1 + prng.nextInt(maxQuantity)
  }

  def decideBidQuantity(): Int = {
    1 + prng.nextInt(maxQuantity)
  }

  def decideInstrument(): String = {
    val idx = prng.nextInt(assets.size)
    assets.keys.toList(idx)
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

  override def preStart(): Unit = {
    self ! StartTrading
  }

  def receive: Receive = {
    case StartTrading =>
      market ! generateNewOrder()
    case OrderReceived =>
      market ! generateNewOrder()
  }

}


case object StartTrading
