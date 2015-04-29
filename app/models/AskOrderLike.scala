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

/** Trait representing an Ask order.
  *
  * An Ask order is an order to sell a security. The AskOrderLike trait should
  * be mixed in with each specific type of order (i.e., limit orders, market
  * orders, etc).
  *
  */
trait AskOrderLike extends OrderLike {

  val buy = false

  /** Whether or not the ask order crosses some bid order. */
  def crosses(other: BidOrderLike): Boolean

  /** Price formation rules. */
  def formPrice(other: BidOrderLike): Double

  /** AskOrders will often need to be split during the matching process. */
  def split(newQuantity: Double): AskOrderLike

}


object AskOrderLike {

  implicit val ordering: Ordering[AskOrderLike] = Ordering.fromLessThan {
    case (existing: LimitAskOrder, incoming: LimitAskOrder) =>
      incoming.limitPrice < existing.limitPrice  // LimitAskOrder with lower limit price get priority
  }

}
