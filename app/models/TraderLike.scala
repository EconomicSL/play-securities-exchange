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

import scala.collection.mutable


trait TraderLike {
  this: Actor with ActorLogging =>

  val outstandingOrders: mutable.Buffer[UUID]

  val market: ActorRef

  val traderLikeBehavior: Receive = {
    case StartTrading =>
      val orderId = generateNewOrderId()
      market ! generateNewOrder(orderId)
    case OrderAccepted(orderId) =>
      outstandingOrders += orderId
      market ! generateNewOrder(orderId)
    case OrderFilled(orderId) =>
      val idx = outstandingOrders.indexOf(orderId)
      outstandingOrders.remove(idx)
      //market ! generateNewOrder(orderId)
    case OrderRejected(orderId) =>
      throw new RuntimeException("Gasp! An order has been rejected!")
  }

  def cancelExistingOrder(id: UUID): Unit = {
    market ! CancelOrder(id)
  }

  def decideAskPrice(): Double

  def decideBidPrice(): Double

  def decideAskQuantity(): Double

  def decideBidQuantity(): Double

  def generateNewAskOrder(id: UUID): AskOrderLike

  def generateNewBidOrder(id: UUID): BidOrderLike

  def generateNewOrder(id: UUID): OrderLike

  def generateNewOrderId(): UUID = {
    UUID.randomUUID()
  }

}


case object StartTrading


case class CancelOrder(id: UUID)

