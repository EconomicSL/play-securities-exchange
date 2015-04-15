package models

import akka.actor.Actor

import scala.collection.mutable


trait TraderLike {
  this: Actor =>

  def cash: Double

  def assets: mutable.Map[String, Int]

  def decideAskPrice(): Double

  def decideBidPrice(): Double

  def decideAskQuantity(): Int

  def decideBidQuantity(): Int

  def generateNewAskOrder(): AskOrderLike

  def generateNewBidOrder(): BidOrderLike

}
