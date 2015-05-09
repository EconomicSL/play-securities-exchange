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
import scala.util.{Failure, Success, Try}


trait AssetsHolderLike {
  this: Actor with ActorLogging =>

  /* For now assume that AssetsHolderLike can take negative asset positions. */
  val assets: mutable.Map[AssetLike, Double] = mutable.Map[AssetLike, Double]().withDefaultValue(0.0)

  /* Increments an actor's cash holdings. */
  def hoard(amount: Double): Unit = {
    assets(Currency) += amount
  }

  /* Decrements an actor's cash holdings. */
  def dishoard(amount: Double): Try[Payment] = {
    if (assets(Currency) >= amount) {
      assets(Currency) -= amount
      Success(Payment(amount))
    } else {
      Failure(InsufficientFundsException())
    }
  }

  /* Increment actor's securities holdings. */
  def accumulate(asset: AssetLike, quantity: Double): Unit = {
    assets(asset) += quantity
  }

  /* Decrement actor's securities holdings. */
  def deccumulate(asset: AssetLike, quantity: Double): Try[Assets] = {
    if (assets(asset) >= quantity) {
      assets(asset) -= quantity
      Success(Assets(asset, quantity))
    } else {
      Failure(InsufficientAssetsException())
    }
  }

  def assetsHolderBehavior: Receive = {
    case Payment(amount) =>
      hoard(amount)
    case PaymentRequest(amount) =>
      val payment = dishoard(amount)
      sender() ! payment
    case AssetsRequest(asset, quantity) =>
      val assets = deccumulate(asset, quantity)
      sender() ! assets
    case Assets(asset, quantity) =>
      accumulate(asset, quantity)
  }

}


case class AssetsRequest(asset: AssetLike, quantity: Double) extends SellerRequestLike {

  require(quantity > 0.0)

}


case class PaymentRequest(amount: Double) extends BuyerRequestLike {

  require(amount > 0.0)

}


case class InsufficientFundsException(message: String = "Buyer has insufficient funds.") extends Exception(message)


case class InsufficientAssetsException(message: String = "Seller has insufficient assets.") extends Exception(message)
