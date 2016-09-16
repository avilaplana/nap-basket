package com.netaporter.domain

import scala.io.Source._
import scala.util.matching.Regex

case class Inventory(val products: Seq[Product] = Seq.empty[Product]) {

  def find(id: String) = products.find(_.productId == id)

  override def toString = "Inventory:\n" + products.map { p => s"$p" }.mkString("\n")
}


object Inventory {
  val productRegEx = new Regex("^(\\d+),(.+),\\s*£(\\d+(.\\d+)*)$", "productId", "productName", "price")

  def apply(filePath: String): Inventory = {

    val productsAsString = fromInputStream(getClass.getResourceAsStream(filePath)).getLines().toSeq

    val products = productsAsString.tail.map {
      l => productRegEx findFirstMatchIn l map {
        m => Product(m.group("productId"), m.group("productName").trim, BigDecimal(m.group("price")))
      }
    }.flatten


    Inventory(products)
  }
}
