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

import akka.actor.ActorRef


sealed trait TransactionLike {

  val askTradingPartyRef: ActorRef

  val bidTradingPartyRef: ActorRef

  val instrument: AssetLike

  val price: Double

  val quantity: Double

}


/** Represents a cleared transaction between a buyer and a seller.
  *
  * @param askTradingPartyRef: ActorRef of the seller.
  * @param bidTradingPartyRef: ActorRef of the buyer.
  * @param price: Agreed price for the transaction.
  * @param quantity: Agreed quantity for the transaction.

  */
case class Transaction(askTradingPartyRef: ActorRef,
                       bidTradingPartyRef: ActorRef,
                       instrument: AssetLike,
                       price: Double,
                       quantity: Double) extends TransactionLike {

  require(price > 0, "Price must be strictly positive.")
  require(quantity > 0, "Quantity must be strictly positive.")

  override def toString: String = {
    s",${askTradingPartyRef.path.name},${bidTradingPartyRef.path.name},$getClass,$instrument,$price,$quantity"
  }

}
