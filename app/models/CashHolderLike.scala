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

import akka.actor.Actor


/** CashHolder trait
  *
  * CashHolder trait should be mixed in with any Actor that needs to process
  * cash transactions.
  */
trait CashHolderLike {
  this: Actor =>

  /* Actor's cash holdings. */
  var cash: Double

  /* Decrements an actor's cash holdings. */
  def dishoardCash(amount: Double): Unit = {
    cash -= amount
  }

  /* Increments an actor's cash holdings. */
  def hoardCash(amount: Double): Unit = {
    cash += amount
  }

  val cashHolderBehavior: Receive = {
    case Payment(amount) =>
      hoardCash(amount)
    case RequestPayment(amount) =>
      dishoardCash(amount)
      sender() ! Payment(amount)
  }

}


case class Payment(amount: Double) {

  require(amount > 0.0)

}


case class RequestPayment(amount: Double) {

  require(amount > 0.0)

}


