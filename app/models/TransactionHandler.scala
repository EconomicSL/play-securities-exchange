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

  var buyerResponse: Try[BuyerResponseLike] = Failure(NoBuyerResponseException("Buyer response not received!"))

  var sellerResponse: Try[SellerResponseLike] = Failure(NoSellerResponseException("Seller response not received!"))

  def receive: Receive = {

    case Success(Payment(amount)) => sellerResponse match {
      case Failure(NoSellerResponseException(msg)) =>  // Seller not yet responded: store buyer response for later.
        buyerResponse = Success(Payment(amount))
      case Failure(ex) =>  // Seller failed to deliver: refund payment then die!
        buyer ! Payment(amount)
        self ! PoisonPill
      case Success(assets) =>  // Seller successfully delivered: process transaction then die!
        val transaction = Transaction(seller, buyer, tradable, price, quantity)
        log.info(s",${System.nanoTime()}" + transaction.toString)
        buyer ! assets
        seller ! Payment(amount)
        self ! PoisonPill
    }
    case Failure(InsufficientFundsException(msg)) => sellerResponse match {
      case Failure(NoSellerResponseException(otherMsg)) => // Seller not yet responded: store buyer response for later.
        buyerResponse = Failure(InsufficientFundsException(msg))  // store buyer response
      case Failure(ex) =>  // Seller also failed to deliver: just die!
        self ! PoisonPill
      case Success(assets) =>
        seller ! assets  // refund assets to seller
        self ! PoisonPill
    }
    case Success(Assets(instrument, quantity)) => buyerResponse match {
      case Failure(NoBuyerResponseException(msg)) =>
        sellerResponse = Success(Assets(instrument, quantity))  // store seller response
      case Failure(ex) =>
        seller ! Assets(instrument, quantity)  // refund assets!
      case Success(Payment(amount)) => // process  transaction!
        val transaction = Transaction(buyer, seller, tradable, price, quantity)
        log.info(transaction.toString)
        buyer ! Assets(instrument, quantity)
        seller ! Payment(amount)
    }
    case Failure(InsufficientAssetsException(msg)) => buyerResponse match {
      case Failure(NoBuyerResponseException(otherMsg)) =>
        sellerResponse = Failure(InsufficientAssetsException(msg))  // store seller response
      case Failure(InsufficientFundsException(otherMsg)) =>
        // nothing to refund!
        self ! PoisonPill
      case Success(payment) =>
        buyer ! payment  // refund payment!
        self ! PoisonPill
    }

  }

  case class NoBuyerResponseException(message: String) extends Exception

  case class NoSellerResponseException(message: String) extends Exception

}

