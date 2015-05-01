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


trait TraderLike {
  this: Actor with ActorLogging =>

  val market: ActorRef

  val traderLikeBehavior: Receive = {
    case StartTrading =>
      market ! generateNewOrder()
    case OrderAccepted =>
      market ! generateNewOrder()
    case OrderRejected =>
      new AssertionError("Your order has been rejected!")
  }

  def decideAskPrice(): Double

  def decideBidPrice(): Double

  def decideAskQuantity(): Double

  def decideBidQuantity(): Double

  def generateNewAskOrder(instrument: SecurityLike): AskOrderLike

  def generateNewBidOrder(instrument: SecurityLike): BidOrderLike

  def generateNewOrder(): OrderLike

}


case object StartTrading

