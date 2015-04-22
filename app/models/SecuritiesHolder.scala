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

import scala.collection.mutable


trait SecuritiesHolder {
  this: Actor =>

  /* Actor's securities holdings. */
  val securities: mutable.Map[String, Int]

  /* Decrement actor's securities holdings. */
  def deccumulateSecurities(instrument: String, quantity: Int): Unit = {
    securities(instrument) -= quantity
  }

  /* Increment actor's securities holdings. */
  def accumulateSecurities(instrument: String, quantity: Int): Unit = {
    securities(instrument) += quantity
  }

  def handleSecuritiesTransaction: Receive = {
    case RequestSecurities(instrument, quantity) =>
      deccumulateSecurities(instrument, quantity)
      sender() ! Securities(instrument, quantity)
    case Securities(instrument, quantity) =>
      accumulateSecurities(instrument, quantity)
  }

}


case class RequestSecurities(instrument: String, quantity: Int) {

  require(quantity > 0)

}


case class Securities(instrument: String, quantity: Int) {

  require(quantity > 0)

}

