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


import java.util.UUID
import akka.actor.ActorRef


/** Trait representing an order for a particular Tradable.
  *
  * The OrderLike trait should be mixed in with each specific type of order
  * (i.e., ask orders, bid orders, limit orders, market orders, etc).
  *
  */
trait OrderLike {

  /** Whether the order is buy (true) or sell (false). */
  def buy: Boolean

  /** The unique identifier for the order. */
  def id: UUID

  /** The quantity of the Tradable being bought or sold. */
  def quantity: Double

  /** Orders will often need to be split during the matching process. */
  def split(newQuantity: Double): OrderLike

  /** String representation of an order. */
  def toString: String

  /** Tradable for which the order is being generated. */
  def tradable: SecurityLike

  /** The trading party for the order. */
  def tradingPartyRef: ActorRef

}
