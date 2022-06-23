package com.tkroman.kpi.y2022.l1

import munit.FunSuite
import List.*
import List._

class ListTest extends FunSuite{
  test("getHead on empty") {
    val expected = "NO HEAD!"
    val actual = intercept[RuntimeException] {
      List().getHead
    }
    assertEquals(expected, actual.getMessage)
  }
  test("getHead on some") {
    val expected = 4
    val actual = List(4, 2, 5, 3 , 1).getHead
    assertEquals(actual, expected)
  }
  test("getTail on empty") {
    val expected = List.Nil
    val actual = List().getTail
    assertEquals(actual, expected)
  }
  test("getTail on some") {
    val expected = List(3, 5, 4, 1)
    val actual = List(2, 3, 5, 4, 1).getTail
    assertEquals(actual, expected)
  }
  test("length on empty") {
    val expected = 0
    val actual = List().length
    assertEquals(actual, expected)
  }
  test("length on some") {
    val expected = 5
    val actual = List(5, 1, 2, 4, 3).length
    assertEquals(actual, expected)
  }
  test("reverse on empty") {
    val expected = List.Nil
    val actual = List().reverse
    assertEquals(actual, expected)
  }
  test("reverse on some") {
    val expected = List(5, 4, 3, 2, 1)
    val actual = List(1, 2, 3, 4, 5).reverse
    assertEquals(actual, expected)
  }
  test("concatenate on empty") {
    val expected = List.Nil
    val actual = List().concatenate(List())
    assertEquals(actual, expected)
  }
  test("concatenate on some") {
    val expected = List(1, 2, 3, 7, 8, 9)
    val actual = List(1, 2, 3).concatenate(List(7, 8, 9))
    assertEquals(actual, expected)
  }
  test("partition on empty") {
    val expected = "NO HEAD!"
    val actual = intercept[RuntimeException] {
      List().partition({ (a: Int, b: Int) =>
      if a > b then Compared.Gt
      else if a < b then Compared.Lt
      else Compared.Eq
    })
    }
    assertEquals(expected, actual.getMessage)
  }
  test("partition on some") {
    val expected = (List(2, 1, 3), List(5, 4))
    val actual = List(3, 5, 2, 1, 4).partition({ (a: Int, b: Int) =>
      if a > b then Compared.Gt
      else if a < b then Compared.Lt
      else Compared.Eq
    })
    assertEquals(actual, expected)
  }
  test("quickSort on empty") {
    val expected = List.Nil
    val actual = List().quickSort({ (a: Int, b: Int) =>
      if a > b then Compared.Gt
      else if a < b then Compared.Lt
      else Compared.Eq
    })
    assertEquals(actual, expected)
  }
  test("quickSort on some") {
    val expected = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
     13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
     28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 
     43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 
     58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 
     73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 
     88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100
    )
    val actual = List(59, 20, 95, 53, 28, 27, 11, 15, 34, 41, 
     7, 77, 64, 55, 98, 96, 90, 84, 91, 69, 8, 72, 80, 9, 78, 
     57, 35, 86, 49, 92, 63, 62, 21, 83, 65, 100, 67, 85, 39, 
     82, 31, 5, 18, 71, 87, 1, 97, 10, 79, 43, 16, 68, 40, 56, 
     74, 73, 61, 14, 30, 44, 81, 48, 24, 42, 58, 50, 94, 38, 36, 
     33, 54, 22, 88, 25, 45, 70, 3, 60, 89, 17, 52, 26, 6, 32, 76, 
     47, 2, 4, 93, 12, 37, 99, 51, 75, 23, 29, 66, 19, 13, 46 
    ).quickSort({ (a: Int, b: Int) =>
      if a > b then Compared.Gt
      else if a < b then Compared.Lt
      else Compared.Eq
    })
    assertEquals(actual, expected)
  }
  test("flatMap on Int") {
    val expected = List(2, 1, 3, 2, 4, 3, 5, 4)
    val actual = List(1, 2, 3, 4).flatMap(s => List.of(s + 1, s))
    assertEquals(actual, expected)
  }
  test("flatMap on String") {
    val expected = List("aHello", "World!", "bHello", "World!", "cHello", "World!")
    val actual = List("a","b","c").flatMap(s => List.of(s + "Hello", "World!"))
    assertEquals(actual, expected)
  }
  test("map on Int") {
    val expected = List(2, 4, 6, 8)
    val actual = List(1, 2, 3, 4).map(s => s * 2)
    assertEquals(actual, expected)
  }
  test("map on String") {
    val expected = List("aHello World!","bHello World!", "cHello World!")
    val actual = List("a","b","c").map(s => s + "Hello World!")
    assertEquals(actual, expected)
  }
  test("tails on empty") {
    val expected = List.Nil
    val actual = List().tails
    assertEquals(actual, expected)
  }
  test("tails on some") {
    val expected = List(
     List(5, 3, 2, 1, 4), 
     List(3, 2, 1, 4), 
     List(2, 1, 4), 
     List(1, 4), 
     List(4)
     )
    val actual = List(5, 3, 2, 1, 4).tails
    assertEquals(actual, expected)
  }
  test("tostring on empty") {
    val expected = "[]"
    val actual = List().toString
    assertEquals(actual, expected)
  }
  test("product on empty") {
    val expected = List(List())
    val actual = product(List())
    assertEquals(actual, expected)
  }
  test("product on some") {
    val expected = List(
     List(1, 3, 5), List(1, 3, 6), 
     List(1, 4, 5), List(1, 4, 6), 
     List(2, 3, 5), List(2, 3, 6), 
     List(2, 4, 5), List(2, 4, 6)
     )
    val actual = product(List(List(1, 2), List(3, 4), List(5, 6)))
    assertEquals(actual, expected)
  }
}