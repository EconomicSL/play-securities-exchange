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

import akka.actor.{ActorLogging, Actor, ActorRef, Props}
import akka.agent.Agent
import akka.routing._
import scala.collection.{immutable, mutable}


/** Represents a security exchange.
  *
  * A security Exchange receives various types of ask and bid orders from
  * traders and efficiently routes them to the correct market for further
  * processing.
  *
  */
class SecuritiesExchange extends ExchangeLike
  with SecuritiesProvider {

  /** Create a double auction market actor for each security in tickers. */
  val markets = securities map {
    case security => (security, context.actorOf(Props[DoubleAuctionMechanism](
      new DoubleAuctionMechanism(clearingMechanism, security)), security.name))
  }

  /** Exchange has a custom router for incoming orders. */
  val orderRouter = Router(OrderRoutingLogic)

  /** Exchange acts as a router for incoming orders from traders.
    *
    * Exchange routes all valid orders to the appropriate market for further
    * processing; if exchange receives an invalid order, then that order is
    * rejected and returned to the trader who sent it.
    *
    */
  def receive: Receive = {
    case order: OrderLike if ! markets.contains(order.instrument) => sender() ! OrderRejected
    case order: OrderLike => sender() ! OrderAccepted; orderRouter.route(order, sender()); log.info(s",${System.nanoTime()}" + order.toString)
  }

  /** Order routing logic for the Exchange. */
  object OrderRoutingLogic extends RoutingLogic {

    /** Route order for a security to the market on which security is traded. */
    def select(message: Any, routees: immutable.IndexedSeq[Routee]): Routee = {
      message match {
        case order: OrderLike => ActorRefRoutee(markets(order.instrument))
      }
    }

  }

}

case object OrderAccepted

case object OrderRejected
