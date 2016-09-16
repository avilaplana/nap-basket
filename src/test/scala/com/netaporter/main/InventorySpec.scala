package com.netaporter.main

import com.netaporter.domain.Product
import com.netaporter.service.Inventory
import org.scalatest.{Matchers, WordSpec}

class InventorySpec extends WordSpec with Matchers {

  "Inventory" should {
    "be created from a csv file" in {
      val inventory = Inventory("/items.csv")
      inventory.products shouldBe Set(
        Product("1", "Short Sleeve Jumper", 9.99D),
        Product("2", "Shoulder Bag", 9.99D),
        Product("3", "Skinny Jeans", 45.00),
        Product("4", "Leather Jeans", 80),
        Product("5", "Leather Jacket", 199.99),
        Product("6", "Wool Socks", 20.50),
        Product("7", "Piqué Polo shirt", 50.55)
      )
    }
  }
}
