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

/** Trait representing an Bid order.
  *
  * A Bid order is an order to buy a security. The BidOrderLike trait should
  * be mixed in with each specific type of order (i.e., limit orders, market
  * orders, etc).
  *
  */
trait BidOrderLike extends OrderLike {

  val buy = true

  /** Whether or not the bid order crosses some ask order. */
  def crosses(other: AskOrderLike): Boolean

  /** Price formation rules. */
  def formPrice(other: AskOrderLike): Double

  /** BidOrders will often need to be split during the matching process. */
  def split(newQuantity: Double): BidOrderLike

}


object BidOrderLike {

  implicit val ordering: Ordering[BidOrderLike] = Ordering.fromLessThan {
    case (existing: LimitBidOrder, incoming: LimitBidOrder) =>
      incoming.limitPrice > existing.limitPrice // LimitBidOrder with higher limit price get priority
   }

}
