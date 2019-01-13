package com.shopping.cart

import com.typesafe.scalalogging._

class ShoppingCart(item: Item){
  val product :Item = item
  var quantity :Int = 0
}

object ShoppingCart extends LazyLogging{

  /**
    * This function calculates the totalBillAmount (in Pence) for the ShoppingCart.
    * @param item List of ShoppingCart Objects with Product and Quantity.
    * @return Int type totalBill = total amount in Pence for input Item List.
    */
  def checkOut(item: List[ShoppingCart]):Int = {

    var totalBillAmount :Int = 0

    val resultTotalBill = item.map{
      case (item) => {totalBillAmount += totalAmountForItem(item)}
    }
    totalBillAmount
  }

  /**
    * This function contains the itemPrice (per unit) List for all items in shop.
    * @param item - ShoppingCart object that contains product and quantity
    * @return the total price for quantity of the Item Object passed
    */
  def totalAmountForItem(item: ShoppingCart) : Int = {

    val itemPrice:List[(Item, Int)] = List((Apple, 60),(Orange, 25))
    val pricePerUnit = itemPrice.find(_._1 == item.product).get._2

    logger.info(s"totalAmountForItem : Total for ${item.quantity} ${item.product} is ${item.quantity * pricePerUnit}")

    item.quantity * pricePerUnit
  }

  /**
    * Main function. This is the start point of the program.
    *
    * inputList is the input to this program.
    * inputList is passed to function inputToShoppingCartList() that returns List of ShoppingCart Objects
    * this list is then passed to function checkOut() which return the totalBill amount in Pence.
    * @param args
    */
  def main(args:Array[String]):Unit ={

    logger.info("main : Shopping cart coding exercise start !!!")

    val inputList:List[Item] = List(Apple, Apple, Orange, Apple)

    //totalBill is total amount payable in Pence
    val totalBill :Int = checkOut(inputToShoppingCartList(inputList))

    //totalBillAmount is total amount payable in GBP
    val totalBillAmount :Float = totalBill / 100.toFloat

    println(s"Total Bill Amount = $totalBillAmount")

  //end of main
  }

  /**
    * This method converts the input list of products to list of ShoppingCart Objects.
    * @param inputList - Input list of items
    * @return List of ShoppingCart objects with updated quantity is returned to function checkOut()
    */
  def inputToShoppingCartList(inputList :List[Item]) : List[ShoppingCart]= {

    val noOfApples = new ShoppingCart(Apple)
    val noOfOranges = new ShoppingCart(Orange)

    inputList.map{
      case Apple => noOfApples.quantity += 1
      case Orange => noOfOranges.quantity += 1
      case _ => throw new UnsupportedOperationException
    }

    logger.info(s"inputToShoppingCartList : Apple(s) ${noOfApples.quantity} Orange(s) ${noOfOranges.quantity}")
    noOfApples :: noOfOranges :: Nil
  }
//end of Object ShoppingCart
}