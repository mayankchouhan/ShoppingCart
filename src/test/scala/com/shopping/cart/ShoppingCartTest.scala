package com.shopping.cart

import com.shopping.cart.ShoppingCart.{checkOut, inputToShoppingCartList}
import org.scalatest.FunSuite

class ShoppingCartTest extends FunSuite {

  // Epsilon for double value comparison
  //val Eps = 1e-3

  val inpList1 :List[Item] = List(Apple, Apple, Apple, Apple, Apple, Apple)
  val inpList2 :List[Item] = List(Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange)
  val inpList3 :List[Item] = List(Apple, Orange, Apple, Orange, Apple, Orange, Apple, Apple, Orange)
  val inpList4 :List[Item] = List()

  test("test checkOut with 6 Apples and 0 Orange") {
    val sixApplesZeroOrange = checkOut(inputToShoppingCartList(inpList1))
    assert(sixApplesZeroOrange == 360)
  }

  test("test checkOut with 0 Apple and 9 Oranges") {
    val zeroAppleNineOranges = checkOut(inputToShoppingCartList(inpList2))
    assert(zeroAppleNineOranges == 225)
  }

  test("test checkOut with 6 Apples and 9 Oranges") {
    val sixApplesNineOranges = checkOut(inputToShoppingCartList(inpList1:::inpList2))
    assert(sixApplesNineOranges == 585)
  }

  test("test checkOut with 5 Apples and 4 Oranges") {
    val fiveApplesFourOranges = checkOut(inputToShoppingCartList(inpList3))
    assert(fiveApplesFourOranges == 400)
  }

  test("test with empty shopping cart") {
    val emptyShoppingCart = checkOut(inputToShoppingCartList(inpList4))
    assert(emptyShoppingCart == 0)
  }

  test("test checkOut with 1 Apple") {
    val oneApple = checkOut(inputToShoppingCartList(List(Apple)))
    assert(oneApple == 60)
  }

  test("test checkOut with 1 Orange") {
    val oneOrange = checkOut(inputToShoppingCartList(List(Orange)))
    assert(oneOrange == 25)
  }

}
