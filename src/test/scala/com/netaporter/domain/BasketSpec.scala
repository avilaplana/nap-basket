package com.netaporter.domain

import com.netaporter.domain.Basket._
import org.scalatest.{Matchers, WordSpec}


class BasketSpec extends WordSpec with Matchers {

  "add" should {

    "return a immutable Basket with Product1 and quantity 1 when Product1 is added to empty basket" in {
      implicit val find: String => Option[Product] = id => Some(ProductBuilder(id))

      emptyBasket.add("1") should be(
        Right(
          Basket(Map(ProductBuilder("1") -> 1))
        )
      )
    }

    "return a immutable Basket with Product1 and quantity 2 when add Product1 is added to a Basket with Product1 and quantity 1" in {
      implicit val find: String => Option[Product] = id => Some(ProductBuilder(id))

      val basketWithProduct1Once = Basket(Map(ProductBuilder("1") -> 1))
      val basketWithProduct1Twice = basketWithProduct1Once.add("1")
      basketWithProduct1Twice should be(
        Right(
          Basket(Map(ProductBuilder("1") -> 2)))
      )
    }

    "return ProductNotValid error when Product not valid is added to a Basket" in {
      implicit val find: String => Option[Product] = id => None
      emptyBasket.add("1") should be(Left(ProductNotValid))
    }
  }

  "remove" should {
    "return a immutable empty Basket when Product1 is removed from a Basket with Product1 and quantity 1" in {
      implicit val find: String => Option[Product] = id => Some(ProductBuilder(id))

      Basket(Map(ProductBuilder("1") -> 1)).remove("1") should be(Right(emptyBasket))
    }

    "return a immutable Basket with Product1 and quantity 1 when Product1 is removed from a Basket with Product1 and quantity 2" in {
      implicit val find: String => Option[Product] = id => Some(ProductBuilder(id))

      val basketWithProduct1Twice = Basket(Map(ProductBuilder("1") -> 2))
      val basketWithProduct1Once = basketWithProduct1Twice.remove("1")
      basketWithProduct1Once should be(Right(Basket(Map(ProductBuilder("1") -> 1))))
    }

    "return ProductNotValid error when Product not valid is removed from a Basket" in {
      implicit val find: String => Option[Product] = id => None

      Basket(Map(ProductBuilder("1") -> 2)).remove("2") should be(Left(ProductNotValid))
    }

    "return ProductNotFound error when valid Product is removed from a Basket but does not exist in the Basket" in {
      implicit val find: String => Option[Product] = id => Some(ProductBuilder(id))

      Basket(Map(ProductBuilder("1") -> 2)).remove("2") should be(Left(ProductNotFound))
    }
  }

  "total" should {
    "return price of the basket" in {
      Basket(
        Map(
          ProductBuilder("1", 10.45) -> 2,
          ProductBuilder("2", 12.45) -> 3,
          ProductBuilder("3", 78.00) -> 1)
      ).total should be(136.25)
    }
  }
}


object ProductBuilder {
  def apply(id: String) = Product(id, "some name", 10D)

  def apply(id: String, price: BigDecimal) = Product(id, "some name", price)
}
