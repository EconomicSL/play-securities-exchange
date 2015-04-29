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

import akka.actor.{ActorRef, ActorLogging, Actor}


/** Base trait for all Market classes.
  *
  * @note Market has two functions: price and quantity formation (i.e., generating
  *       transactions) via some AuctionMechanismLike; processing of transactions
  *       via some ClearingMechanismLike.
  */
trait MarketLike extends Actor
  with ActorLogging {

  /** Auction mechanism used to generate transaction prices and quantities. */
  def auctionMechanism: ActorRef

  /** Clearing mechanism used to process transactions. */
  def clearingMechanism: ActorRef

}
