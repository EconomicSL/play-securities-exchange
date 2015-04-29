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

import com.typesafe.config.ConfigFactory

import scala.util.Random


/** Create securities given information in configuration file. */
trait SecuritiesProvider extends {

  private val securitiesConf = ConfigFactory.load("securities.conf")

  val securities: List[Security] = {
    val prng = new Random(securitiesConf.getInt("securities.seed"))
    val maxSharesOutstanding = securitiesConf.getInt("securities.maxSharesOutstanding")
    val symbols = securitiesConf.getStringList("securities.symbols")
    for (symbol <- symbols) yield Security(symbol, prng.nextInt(maxSharesOutstanding))
  }

}
