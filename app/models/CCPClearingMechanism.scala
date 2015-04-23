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

import akka.actor.{Props, ActorLogging, Actor}

import scala.collection.mutable


/** Central counter party (CCP) clearing mechanism.
  *
  * Central counter party inserts itself as trading party with ask and bid actors.
  * 
  */
class CCPClearingMechanism extends Actor with
  ActorLogging with
  CashHolder with
  SecuritiesHolder {

  /* For now assume that central counter party has "deep pockets". */
  var cash: Double = Double.PositiveInfinity

  /* For now assume that central counter party can take negative asset positions. */
  val securities: mutable.Map[String, Int] = mutable.Map[String, Int]().withDefaultValue(0)

  /** Central counter-party (CCP) clearing mechanism behavior
    *
    * @note The key difference between CCP clearing and bilateral clearing is that
    * CCP inserts itself as the counter-party to both the ask and the bid
    * trading parties before processing the final transaction. By acting as
    * a counter-party to every transaction, the CCP assumes all counter-party
    * risk.
    */
  val clearingMechanismBehavior: Receive = {
    case PartialFill(askTradingPartyRef, bidTradingPartyRef, instrument, price, quantity) =>
      // insert self as counter party to the bid trading party
      val askTransactionHandler = context.actorOf(Props[TransactionHandler])
      askTransactionHandler ! PartialFill(self, bidTradingPartyRef, instrument, price, quantity)

      // insert self as counter party to the ask trading party
      val bidTransactionHandler = context.actorOf(Props[TransactionHandler])
      bidTransactionHandler ! PartialFill(askTradingPartyRef, self, instrument, price, quantity)

    case TotalFill(askTradingPartyRef, bidTradingPartyRef, instrument, price, quantity) =>
      // insert self as counter party to the bid trading party
      val askTransactionHandler = context.actorOf(Props[TransactionHandler])
      askTransactionHandler ! TotalFill(self, bidTradingPartyRef, instrument, price, quantity)

      // insert self as counter party to the ask trading party
      val bidTransactionHandler = context.actorOf(Props[TransactionHandler])
      bidTransactionHandler ! TotalFill(askTradingPartyRef, self, instrument, price, quantity)
  }

  def receive: Receive = {
    clearingMechanismBehavior orElse cashHolderBehavior orElse securitiesHolderBehavior
  }
  
}
