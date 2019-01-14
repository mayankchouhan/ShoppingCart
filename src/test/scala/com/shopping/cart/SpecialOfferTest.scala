package com.shopping.cart

import com.shopping.cart.ShoppingCart.{checkOut, inputToShoppingCartList}
import org.scalatest.FunSuite

class SpecialOfferTest extends FunSuite {

  val inpList1 :List[Item] = List(Apple, Apple, Apple, Apple, Apple, Apple)
  val inpList2 :List[Item] = List(Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange)
  val inpList3 :List[Item] = List(Apple, Orange, Apple, Orange, Apple, Orange, Apple, Apple, Orange)
  val inpList4 :List[Item] = List()

  test("test checkOut with 6 Apples(Offer 2 for 1) and 0 Orange") {
    val sixApplesZeroOrange = checkOut(inputToShoppingCartList(inpList1))
    assert(sixApplesZeroOrange == 180)
  }

  test("test checkOut with 0 Apple and 9 Oranges(Offer 3 for 2)") {
    val zeroAppleNineOranges = checkOut(inputToShoppingCartList(inpList2))
    assert(zeroAppleNineOranges == 150)
  }

  test("test checkOut with 6 Apples(Offer 2 for 1) and 9 Oranges(Offer 3 for 2)") {
    val sixApplesNineOranges = checkOut(inputToShoppingCartList(inpList1:::inpList2))
    assert(sixApplesNineOranges == 330)
  }

  test("test checkOut with 5 Apples(Offer 2 for 1) and 4 Oranges(Offer 3 for 2)") {
    val fiveApplesFourOranges = checkOut(inputToShoppingCartList(inpList3))
    assert(fiveApplesFourOranges == 255)
  }

  test("test with empty shopping cart") {
    val emptyShoppingCart = checkOut(inputToShoppingCartList(inpList4))
    assert(emptyShoppingCart == 0)
  }

  test("test checkOut with 1 Apple(Offer 2 for 1)") {
    val oneApple = checkOut(inputToShoppingCartList(List(Apple)))
    assert(oneApple == 60)
  }

  test("test checkOut with 1 Orange(Offer 3 for 2)") {
    val oneOrange = checkOut(inputToShoppingCartList(List(Orange)))
    assert(oneOrange == 25)
  }

}
