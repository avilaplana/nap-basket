package com.netaporter.domain

sealed trait BasketError

case object ProductNotValid extends BasketError

case object ProductNotFound extends BasketError

case class Product(productId: String, productName: String, price: BigDecimal) {
  override def toString = s"Product with id: $productId, name: $productName, price: £$price"
}

case class Basket(private val products: Map[Product, Int]) {

  override def toString = "Basket:\n" + products.map {
    case (product, quantity) => s"$product with quantity: $quantity"
  }.mkString("\n")

  private def immutableBasket(p: Map[Product, Int]) = this.copy(products = p)

  def add(id: String)(implicit find: String => Option[Product]): Either[BasketError, Basket] = find(id) match {
    case Some(p) => products.get(p) match {
      case Some(n) => Right(immutableBasket(products + (p -> (n + 1))))
      case None => Right(immutableBasket(products + (p -> (1))))
    }
    case None => Left(ProductNotValid)
  }

  def remove(id: String)(implicit find: String => Option[Product]): Either[BasketError, Basket] = find(id) match {
    case Some(p) => products.get(p) match {
      case Some(1) => Right(immutableBasket(products - p))
      case Some(n) => Right(immutableBasket(products + (p -> (n - 1))))
      case None => Left(ProductNotFound)
    }
    case None => Left(ProductNotValid)
  }

  def total = products.map {
    case (product, quantity) => product.price * quantity
  }.sum
}

object Basket {
  def emptyBasket = Basket(Map.empty[Product, Int])
}

