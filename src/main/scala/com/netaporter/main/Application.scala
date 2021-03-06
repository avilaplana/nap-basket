package com.netaporter.main

import com.netaporter.domain.Basket._
import com.netaporter.domain.{Inventory, ProductNotFound, ProductNotValid}


object Application extends App {

  println("************************************")
  println("* Welcome to the Net-A-Porter Shop *")
  println("************************************")
  println("Enter \"Q\" to Quit")
  println("Enter \"add <ProductId>\" to add to basket")
  println("Enter \"remove <ProductId>\" to remove from basket")
  println("Enter \"list\" to show a list of products in the inventory")
  println("Enter \"total\" to show the total price of the basket")

  val inventory = Inventory("/items.csv")
  implicit val find = inventory.find _

  var basket = emptyBasket

  val input = io.Source.stdin.getLines.takeWhile(!_.equals("Q")).map(_.split(" ").toList)
  input foreach {
    case "add" :: productId :: Nil =>
      basket.add(productId) match {
        case Right(b) =>
          basket = b
          println(basket)
        case Left(_) => println(s"Product with id: $productId does not exist")
      }

    case "remove" :: productId :: Nil =>
      basket.remove(productId) match {
        case Right(b) =>
          basket = b
          println(basket)
        case Left(ProductNotValid) => println(s"Product with id: $productId does not exist")
        case Left(ProductNotFound) => println(s"Product with id: $productId is not found in the basket")
      }
    case "list" :: Nil =>
      println(inventory)
    case "total" :: Nil =>
      println(basket.total)
    case _ =>
      println(s"Sorry, that is not a valid command")
  }

  println("Thanks for shopping at Net-a-Porter!")

}
