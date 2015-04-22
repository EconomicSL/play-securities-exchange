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


/** Central counter party (CCP) clearing mechanism.
  *
  * Central counter party inserts itself as trading party with ask and bid actors.
  * 
  */
class CCPClearingMechanism extends Actor
  with ActorLogging {

  def receive: Receive = {
    case fill: PartialFill =>
      // insert self as counter party to the bid trading party
      val askTransactionHandler = context.actorOf(Props[TransactionHandler])
      askTransactionHandler ! PartialFill(self, fill.bidTradingPartyRef, fill.instrument, fill.price, fill.quantity)

      // insert self as counter party to the ask trading party
      val bidTransactionHandler = context.actorOf(Props[TransactionHandler])
      bidTransactionHandler ! PartialFill(fill.askTradingPartyRef, self, fill.instrument, fill.price, fill.quantity)

    case fill: TotalFill =>
      // insert self as counter party to the bid trading party
      val askTransactionHandler = context.actorOf(Props[TransactionHandler])
      askTransactionHandler ! TotalFill(self, fill.bidTradingPartyRef, fill.instrument, fill.price, fill.quantity)

      // insert self as counter party to the ask trading party
      val bidTransactionHandler = context.actorOf(Props[TransactionHandler])
      bidTransactionHandler ! TotalFill(fill.askTradingPartyRef, self, fill.instrument, fill.price, fill.quantity)
  }
  
}
