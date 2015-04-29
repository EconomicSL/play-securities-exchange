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


/** Handles clearing of an individual transaction.
  *
  * Reduced form model of a payments system.
  *
  * @todo handle failure to send payment and failure to send securities.
  *
  */
class TransactionHandler extends Actor {

  var buyer: ActorRef = Actor.noSender

  var seller: ActorRef = Actor.noSender

  var payment: Option[Payment] = None

  var securities: Option[Assets] = None

  def paymentReceived: Boolean = {
    payment.isDefined
  }

  def securitiesReceived: Boolean = {
    securities.isDefined
  }

  def receive: Receive = {
    case fill: FillLike =>
      seller = fill.askTradingPartyRef; buyer = fill.bidTradingPartyRef
      seller ! RequestAssets(fill.instrument, fill.quantity)
      buyer ! RequestPayment(fill.price * fill.quantity)
    case Payment(amount) =>
      payment = Some(Payment(amount))
      if (securitiesReceived) {
        buyer ! securities.get
        seller ! payment.get
        context.stop(self)
      }
    case Assets(instrument, quantity) =>
      securities = Some(Assets(instrument, quantity))
      if (paymentReceived) {
        buyer ! securities.get
        seller ! payment.get
        context.stop(self)
      }
  }

}

