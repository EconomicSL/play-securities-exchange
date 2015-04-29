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

import scala.collection.mutable


trait AssetsHolderLike {
  this: Actor with ActorLogging =>

  /* For now assume that AssetsHolderLike can take negative asset positions. */
  val assets: mutable.Map[AssetLike, Double] = mutable.Map[AssetLike, Double]().withDefaultValue(0.0)

  /* Increments an actor's cash holdings. */
  def hoard(amount: Double): Unit = {
    assets(Currency) += amount
  }

  /* Decrements an actor's cash holdings. */
  def dishoard(amount: Double): Unit = {
    assets(Currency) -= amount
  }

  /* Increment actor's securities holdings. */
  def accumulate(asset: AssetLike, quantity: Double): Unit = {
    assets(asset) += quantity
  }

  /* Decrement actor's securities holdings. */
  def deccumulate(asset: AssetLike, quantity: Double): Unit = {
    assets(asset) -= quantity
  }

  def assetsHolderBehavior: Receive = {
    case Payment(amount) =>
      hoard(amount)
    case RequestPayment(amount) =>
      dishoard(amount)
      sender() ! Payment(amount)
    case RequestAssets(asset, quantity) =>
      deccumulate(asset, quantity)
      sender() ! Assets(asset, quantity)
    case Assets(asset, quantity) =>
      accumulate(asset, quantity)
  }

}


case class RequestAssets(asset: AssetLike, quantity: Double) {

  require(quantity > 0.0)

}


case class Assets(instrument: AssetLike, quantity: Double) {

  require(quantity > 0.0)

}


case class Payment(amount: Double) {

  require(amount > 0.0)

}


case class RequestPayment(amount: Double) {

  require(amount > 0.0)

}
