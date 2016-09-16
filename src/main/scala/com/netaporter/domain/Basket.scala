package com.netaporter.domain

sealed trait BasketError

case object ProductNotValid extends BasketError

case object ProductNotFound extends BasketError

case class Product(productId: String, productName: String, price: Double)

case class Basket(private val products: Map[Product, Int]) {

  //  private def c(id: String)(exist: String => Option[Product])(action: Product => Basket) = exist(id) match {
  //    case Some(p) => Right(action)
  //    case None => Left(ProductNotValid)
  //  }
  private def immutableBasket(p: Map[Product, Int]) = this.copy(products = p)

  def add(id: String)(implicit exist: String => Option[Product]): Either[BasketError, Basket] = exist(id) match {
    case Some(p) => products.get(p) match {
      case Some(n) => Right(immutableBasket(products + (p -> (n + 1))))
      case None => Right(immutableBasket(products + (p -> (1))))
    }
    case None => Left(ProductNotValid)
  }

  def remove(id: String)(implicit exist: String => Option[Product]): Either[BasketError, Basket] = exist(id) match {
    case Some(p) => products.get(p) match {
      case Some(1) => Right(immutableBasket(products - p))
      case Some(n) => Right(immutableBasket(products + (p -> (n - 1))))
      case None => Left(ProductNotFound)
    }
    case None => Left(ProductNotValid)
  }

  def total = products.map {
    case (p, quantity) => p.price * quantity
  }.sum
}

object Basket {
  def emptyBasket = Basket(Map.empty[Product, Int])
}

