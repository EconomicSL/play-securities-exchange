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

import akka.actor._

import scala.util.{Failure, Success, Try}


object TransactionHandler {

  def props(buyer: ActorRef,
            seller: ActorRef,
            tradable: AssetLike,
            price: Double,
            quantity: Double): Props = {
    Props(new TransactionHandler(buyer, seller, tradable, price, quantity))
  }

}

/** Handles clearing of an individual transaction.
  *
  * Reduced form model of a payments system.
  *
  */
class TransactionHandler(buyer: ActorRef,
                         seller: ActorRef,
                         tradable: AssetLike,
                         price: Double,
                         quantity: Double) extends Actor
  with ActorLogging {

  seller ! AssetsRequest(tradable, quantity)
  buyer ! PaymentRequest(price * quantity)

  /* Only evaluated if necessary! */
  lazy val transaction = Transaction(seller, buyer, tradable, price, quantity)

  def awaitingBuyerResponse(sellerResponse: Try[Assets]): Receive = sellerResponse match {
    case Success(assets) => {  // partial function for handling buyer response given successful seller response
      case Success(payment) =>
        log.info(s",${System.nanoTime()}" + transaction.toString)
        buyer ! assets
        seller ! payment
        self ! PoisonPill
      case Failure(ex) =>
        seller ! assets  // refund assets to seller
        self ! PoisonPill
    }
    case Failure(exception) => {  // partial function for handling buyer response given failed seller response
      case Success(payment) =>  // refund payment to buyer
        buyer ! payment
        self ! PoisonPill
      case Failure(otherException) => // nothing to refund
        self ! PoisonPill
    }
  }

  def awaitingSellerResponse(buyerResponse: Try[Payment]): Receive = buyerResponse match {
    case Success(payment) => {  // partial function for handling seller response given successful buyer response
      case Success(assets) =>
        log.info(s",${System.nanoTime()}" + transaction.toString)
        buyer ! assets
        seller ! payment
        self ! PoisonPill
      case Failure(exception) =>
        seller ! payment  // refund payment to buyer
        self ! PoisonPill
    }
    case Failure(exception) => {  // partial function for handling seller response given failed buyer response
      case Success(assets) =>  // refund assets to seller
        buyer ! assets
        self ! PoisonPill
      case Failure(otherException) =>  // nothing to refund
        self ! PoisonPill
    }
  }

  def receive: Receive = {

    case Success(Payment(amount)) =>
      context.become(awaitingSellerResponse(Success(Payment(amount))))
    case Failure(InsufficientFundsException(msg)) =>
      context.become(awaitingSellerResponse(Failure(InsufficientFundsException(msg))))
    case Success(Assets(instrument, quantity)) =>
      context.become(awaitingBuyerResponse(Success(Assets(instrument, quantity))))
    case Failure(InsufficientAssetsException(msg)) =>
      context.become(awaitingBuyerResponse(Failure(InsufficientAssetsException(msg))))

  }

}

