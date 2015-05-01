import akka.actor.{Props, ActorSystem}
import models._

import scala.util.Random

object Main extends App
  with SecuritiesProvider {

  val prng = new Random(42)

  def generateRandomAssets(maxQuantity: Double = 1e6): List[Assets] = {
    securities.map(security => Assets(security, generateRandomQuantity(maxQuantity)))
  }

  def generateRandomCurrency(maxQuantity: Double = 1e6): Assets = {
    Assets(Currency, generateRandomQuantity(maxQuantity))
  }

  def generateRandomQuantity(maxQuantity: Double = 1e6): Double = {
    prng.nextDouble() * maxQuantity
  }

  val tradingPlatform = ActorSystem("Trading-Platform")
  val securitiesExchange = tradingPlatform.actorOf(Props[SecuritiesExchange])
  val noiseTrader = tradingPlatform.actorOf(Props(classOf[NoiseTrader], securitiesExchange, prng))


  // Initialize NoiseTrader
  val initialAssets = generateRandomAssets()
  initialAssets foreach(asset => noiseTrader ! asset)

  val initialCurrency = generateRandomCurrency()
  noiseTrader ! initialCurrency

  noiseTrader ! StartTrading

}