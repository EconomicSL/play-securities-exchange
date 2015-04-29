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

/** Represents a financial security
  *
  * A financial instrument that represents: an ownership position in a
  * publicly-traded corporation (stock), a creditor relationship with
  * governmental body or a corporation (bond), or rights to ownership as
  * represented by an option. A SecurityLike is a fungible, negotiable financial
  * instrument that represents some type of financial value.
  */
sealed trait SecurityLike extends AssetLike


/** Represents an ownership position in a publicly-traded corporation.
  *
  * @param symbol Stock ticker symbol.
  */
case class Stock(symbol: String) extends SecurityLike

