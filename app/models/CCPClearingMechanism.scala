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

import akka.actor.{ActorRef, Props}

import scala.collection.mutable


/** Central counter party (CCP) clearing mechanism.
  *
  * @note The key difference between CCP clearing and bilateral clearing is that
  * CCP inserts itself as the counter-party to both the ask and the bid
  * trading parties before processing the final transaction. By acting as
  * a counter-party to every transaction, the CCP assumes all counter-party
  * risk.
  */
class CCPClearingMechanism extends ClearingMechanismLike with
  CashHolderLike with
  AssetsHolderLike {

  /** Central counterparty For now assume that central counter party has "deep pockets". */
  var cash: Double = Double.PositiveInfinity

  /* BilateralClearingMechanism can be used to process novated fills. */
  val bilateralClearingMechanism: ActorRef = context.actorOf(Props[BilateralClearingMechanism])

  /** Central counter-party (CCP) clearing mechanism behavior. */
  val clearingMechanismBehavior: Receive = {
    case fill: FillLike =>
      val novatedFills = novate(fill)
      novatedFills foreach(novatedFill => bilateralClearingMechanism ! novatedFill)
  }

  /** Novate a FillLike between two trading counterparties.
    *
    * @note The substitution of counterparties is typically accomplished through
    *       a legal process called contract novation. Novation discharges the
    *       contract between the original trading counterparties and creates two new,
    *       legally binding contracts – one between each of the original trading
    *       counterparties and the central counterparty.
    * @param fill a FillLike between two trading counterparties.
    * @return a list of two FillLikes - one between each of the original trading
    *         counterparties and the central counterparty.
    */
  def novate(fill: FillLike): List[FillLike] = fill match {
    case fill: PartialFill =>
      List(PartialFill(self, fill.bidTradingPartyRef, fill.instrument, fill.price, fill.quantity),
           PartialFill(fill.askTradingPartyRef, self, fill.instrument, fill.price, fill.quantity))
    case fill: TotalFill =>
      List(TotalFill(self, fill.bidTradingPartyRef, fill.instrument, fill.price, fill.quantity),
           TotalFill(fill.askTradingPartyRef, self, fill.instrument, fill.price, fill.quantity))
  }

  def receive: Receive = {
    clearingMechanismBehavior orElse cashHolderBehavior orElse assetsHolderBehavior
  }
  
}
