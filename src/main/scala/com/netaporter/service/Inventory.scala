package com.netaporter.service

import com.netaporter.domain.Product

import scala.io.Source._
import scala.util.matching.Regex

class Inventory(val products: Set[Product] = Set.empty[Product]) {
  def exist(id: String) = products.find(_.productId == id)
}


object Inventory {
  val productRegEx = new Regex("^(\\d+),(.+),\\s*£(\\d+(.\\d+)*)$", "productId", "productName", "price")

  def apply(filePath: String): Inventory = {

    val productsAsString = fromInputStream(getClass.getResourceAsStream(filePath)).getLines().toSeq

    val products = productsAsString.tail.map {
      l => productRegEx findFirstMatchIn l map {
        m => Product(m.group("productId"), m.group("productName").trim, m.group("price").toDouble)
      }
    }.flatten.toSet


    new Inventory(products)
  }
}
