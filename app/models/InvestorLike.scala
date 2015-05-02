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

import akka.actor.{ActorLogging, Actor}


trait InvestorLike {
  this: Actor with ActorLogging =>

  def investorLikeBehavior: Receive

  def decideSecurity(): SecurityLike

}

// send these messages to the Trader/Broker in order to convey whether to buy or sell a particular instrument

case class AskOrder(instrument: SecurityLike)


case class BidOrder(instrument: SecurityLike)


