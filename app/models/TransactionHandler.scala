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

import akka.actor.{ActorRef, Actor}

import scala.util.{Failure, Success, Try}


/** Handles clearing of an individual transaction.
  *
  * Reduced form model of a payments system.
  *
  */
class TransactionHandler extends Actor {

  var buyer: ActorRef = Actor.noSender

  var seller: ActorRef = Actor.noSender

  var buyerResponse: Try[Payment] = Failure(NoBuyerResponse())

  var sellerResponse: Try[Assets] = Failure(NoSellerResponse())

  def receive: Receive = {
    case mesg: FillLike =>
      seller = mesg.askTradingPartyRef
      buyer = mesg.bidTradingPartyRef
      seller ! RequestAssets(mesg.instrument, mesg.quantity)
      buyer ! RequestPayment(mesg.price * mesg.quantity)

    case mesg: Try[Payment] => mesg match {
      case Success(payment) => sellerResponse match {
        case Failure(NoSellerResponse()) =>
          buyerResponse = mesg  // store buyer response
        case Failure(InsufficientAssetsException()) =>
          buyer ! payment  // refund payment
        case Success(assets) => // process transaction!
          buyer ! assets
          seller ! payment
      }
      case Failure(InsufficientFundsException()) => sellerResponse match {
        case Failure(NoSellerResponse()) =>
          buyerResponse = mesg  // store buyer response
        case Failure(InsufficientAssetsException()) =>
          // nothing to refund!
        case Success(assets) =>
          seller ! assets  // refund assets to seller
      }
    }

    case mesg: Try[Assets] => mesg match {
      case Success(assets) => buyerResponse match {
        case Failure(NoBuyerResponse()) =>
          sellerResponse = mesg  // store seller response
        case Failure(InsufficientFundsException()) =>
          seller ! assets  // refund assets!
        case Success(payment) => // process  transaction!
          buyer ! assets
          seller ! payment
      }
      case Failure(InsufficientAssetsException()) => buyerResponse match {
        case Failure(NoBuyerResponse()) =>
          sellerResponse = mesg  // store seller response
        case Failure(InsufficientFundsException()) =>
          // nothing to refund!
        case Success(payment) =>
          buyer ! payment  // refund payment!
      }
    }
  }

  case class NoBuyerResponse(message: String = "Buyer response not received") extends Exception

  case class NoSellerResponse(message: String = "Buyer response not received") extends Exception

}

