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

import java.util.UUID

import akka.actor.{ActorLogging, ActorRef, Actor}
import com.typesafe.config.ConfigFactory

import scala.util.Random


case class NoiseTrader(market: ActorRef,
                       prng: Random) extends Actor
  with ActorLogging
  with TraderLike
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

  def decideInstrument(): SecurityLike = {
    // find all securities
    val securities = assets.filterKeys(_.isInstanceOf[SecurityLike])

    // pick a security at random
    val idx = prng.nextInt(securities.size)
    securities.keys.toList(idx).asInstanceOf[SecurityLike]
  }

  def generateNewAskOrder(id: UUID): AskOrderLike = {
    LimitAskOrder(id, self, decideInstrument(), decideAskPrice(), decideAskQuantity())
  }

  def generateNewBidOrder(id: UUID): BidOrderLike = {
    LimitBidOrder(id, self, decideInstrument(), decideBidPrice(), decideBidQuantity())
  }

  def generateNewOrder(): OrderLike = {
    // generate an order id
    val id = UUID.randomUUID()

    if (prng.nextDouble() < askOrderProbability) {
      generateNewAskOrder(id)
    } else {
      generateNewBidOrder(id)
    }
  }

  def receive: Receive = {
    traderLikeBehavior orElse assetsHolderBehavior
  }

}