package models

/** Represents a financial security
  *
  * A financial instrument that represents: an ownership position in a
  * publicly-traded corporation (stock), a creditor relationship with
  * governmental body or a corporation (bond), or rights to ownership as
  * represented by an option. A Security is a fungible, negotiable financial
  * instrument that represents some type of financial value.
  */
case class Security(name: String, numberSharesOutstanding: Int)

