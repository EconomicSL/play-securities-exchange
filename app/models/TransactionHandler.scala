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

import akka.actor.{Props, ActorLogging, ActorRef, Actor}

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

  var buyerResponse: Try[BuyerResponseLike] = Failure(NoBuyerResponseException("Buyer response not received!"))

  var sellerResponse: Try[SellerResponseLike] = Failure(NoSellerResponseException("Seller response not received!"))

  def receive: Receive = {

    case Success(Payment(amount)) => sellerResponse match {
      case Failure(NoSellerResponseException(msg)) =>
        buyerResponse = Success(Payment(amount))  // store buyer response
      case Failure(ex) =>
        buyer ! Payment(amount)  // refund payment
      case Success(Assets(instrument, quantity)) => // process transaction!
        val transaction = Transaction(seller, buyer, instrument, amount / quantity, quantity)
        log.info(transaction.toString)
        buyer ! Assets(instrument, quantity)
        seller ! Payment(amount)
    }
    case Failure(InsufficientFundsException(msg)) => sellerResponse match {
      case Failure(NoSellerResponseException(msg)) =>
        buyerResponse = Failure(InsufficientFundsException(msg))  // store buyer response
      case Failure(ex) =>
        // nothing to refund!
      case Success(assets) =>
        seller ! assets  // refund assets to seller
    }
    case Success(Assets(instrument, quantity)) => buyerResponse match {
      case Failure(NoBuyerResponseException(msg)) =>
        sellerResponse = Success(Assets(instrument, quantity))  // store seller response
      case Failure(ex) =>
        seller ! Assets(instrument, quantity)  // refund assets!
      case Success(Payment(amount)) => // process  transaction!
        val transaction = Transaction(seller, buyer, instrument, amount / quantity, quantity)
        log.info(transaction.toString)
        buyer ! Assets(instrument, quantity)
        seller ! Payment(amount)
    }
    case Failure(InsufficientAssetsException(blah)) => buyerResponse match {
      case Failure(NoBuyerResponseException(msg)) =>
        sellerResponse = Failure(InsufficientAssetsException(blah))  // store seller response
      case Failure(InsufficientFundsException(msg)) =>
        // nothing to refund!
      case Success(payment) =>
        buyer ! payment  // refund payment!
    }

  }

  case class NoBuyerResponseException(message: String) extends Exception

  case class NoSellerResponseException(message: String) extends Exception

}

