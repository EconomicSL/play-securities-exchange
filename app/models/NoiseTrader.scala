/*
Copyright 2015 David R. Pugh

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package models

import akka.actor.{ActorLogging, ActorRef, Actor}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable
import scala.util.Random


case class NoiseTrader(var cash: Double,
                       market: ActorRef,
                       prng: Random) extends Actor
  with ActorLogging
  with TraderLike
  with CashHolderLike
  with AssetsHolderLike {

  val conf = ConfigFactory.load("traders.conf")

  val maxPrice: Double = conf.getDouble("maxPrice")
  
  val maxQuantity: Double = conf.getDouble("maxQuantity")

  val askOrderProbability: Double = conf.getDouble("askOrderProbability")

  def decideAskPrice(): Double = {
    prng.nextDouble() * maxPrice
  }

  def decideBidPrice(): Double = {
    prng.nextDouble() * maxPrice
  }

  def decideAskQuantity(): Double = {
    prng.nextDouble() * maxQuantity
  }

  def decideBidQuantity(): Double = {
    prng.nextDouble() * maxQuantity
  }

  def decideInstrument(): AssetLike = {
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

  def receive: Receive = {
    traderLikeBehavior orElse cashHolderBehavior orElse assetsHolderBehavior
  }

}