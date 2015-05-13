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

import akka.actor.Props
import akka.routing._
import scala.collection.immutable


/** Represents a securities exchange.
  *
  * A SecurityExchange receives various types of ask and bid orders from
  * traders and efficiently routes them to the correct market for further
  * processing. If SecuritiesExchange receives an order for an instrument that
  * is not traded on any of its markets, then it rejects the order and informs
  * the trader who sent it.
  */
class SecuritiesExchange extends ExchangeLike
  with SecuritiesProvider {

  /** Create a double auction market actor for each security in tickers. */
  securities.foreach {
    security => context.actorOf(Props(classOf[SecuritiesMarket], security), security.symbol)
  }

  /** Exchange has a custom router for incoming orders. */
  val router = Router(OrderRoutingLogic)

  /** Order routing logic for the Exchange. */
  object OrderRoutingLogic extends RoutingLogic {

    /** Route order for a security to the market on which security is traded. */
    def select(message: Any, routees: immutable.IndexedSeq[Routee]): Routee = {
      message match {
        case order: OrderLike => ActorRefRoutee(context.child(order.tradable.symbol).get)
      }
    }

  }

  def receive: Receive = {
    case order: OrderLike => context.child(order.tradable.symbol) match {
      case Some(market) =>  // exchange has a market for the instrument
        router.route(order, sender())
        sender() ! OrderAccepted
      case None =>  // exchange does not have a market for the instrument
        sender() ! OrderRejected
    }
  }

}


case object OrderAccepted

case object OrderRejected
