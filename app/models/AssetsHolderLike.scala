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

  /* Decrement actor's securities holdings. */
  def deccumulate(asset: AssetLike, quantity: Double): Unit = {
    assets(asset) -= quantity
  }

  /* Increment actor's securities holdings. */
  def accumulate(asset: AssetLike, quantity: Double): Unit = {
    assets(asset) += quantity
  }

  def assetsHolderBehavior: Receive = {
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

