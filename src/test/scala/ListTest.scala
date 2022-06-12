package com.tkroman.kpi.y2022.l1
import munit.FunSuite
import scala.collection.mutable

import List.*

class ListTest extends FunSuite {
  test("toString function"){
    val expected = "[1, 2, 3, 4]"
    val actual = List.of(1,2,3,4).toString
    assertEquals(expected,actual)
  }

  test("reverse function"){
    val expected = List.of("d", "c", "b", "a")
    val actual = List.of("a","b","c","d").reverse
    assertEquals(expected,actual)
  }

  test("concatenate function"){
    val expected = List.of(1,2,3,4,5,6,7,8)
    val list_1 = List.of(1,2,3,4)
    val list_2 = List.of(5,6,7,8)
    val actual = list_1.concatenate(list_2)
    assertEquals(expected,actual)
  }

  test("foldLeft function (Int)"){
    val expected = 240
    val actual = List.of(1,2,3,4).foldLeft(10)(_*_)
    assertEquals(expected,actual)
  }

  test("foldLeft function (String)"){
    val expected = "eabcd"
    val actual = List.of("a","b", "c", "d").foldLeft("e")(_+_)
    assertEquals(expected,actual)
  }

  test("flatMap function (Int)"){
    val expected = List.of(2, 1, 3, 2, 4, 3, 5, 4)
    val actual = List.of(1,2,3,4).flatMap(s => List.of(s + 1, s))
    assertEquals(expected, actual)
  }

  test("flatMap function (String"){
    val expected = List.of("aHello", "aHell", "World", "bHello", "bHell", "World", "cHello", "cHell", "World")
    val actual = List.of("a","b","c").flatMap(s => List.of(s + "Hello", s + "Hell", "World"))
    assertEquals(expected, actual)
  }

  test("zipWithIndex function (Int)"){
    val expected = List.of((1,0), (2,1), (3,2), (4,3))
    val actual = List.of(1,2,3,4).zipWithIndex
  }

  test("zipWithIndex function (String)"){
    val expected = List.of(("a",0), ("b",1), ("c",2))
    val actual = List.of("a","b","c").zipWithIndex
  }

  test("update function (Int)"){
    val expected = List.of(10, 2, 3, 4)
    val actual = List.of(1,2,3,4).updated(10,0)
  }

  test("update function (String)"){
    val expected = List.of("a", "b", "c", "d", "e")
    val actual = List.of("a","b","c","d").updated("e",4)
  }

  test("collect function (Int)"){
    val expected = List.of(3, 4)
    val actual = List.of(1,2,3,4).collect(x => if (x < 2) None else Some(x))
  }

  test("collect function (String)"){
    val expected = List.of("opqrst")
    val actual = List.of("abc","defg","higkl","opqrst").collect(x => if (x.length < 5) None else Some(x))
  }

  test("unpack function"){
    val expected = List.of(1,2,3,4,5,6,7,8)
    val actual = List(List(1,2,3,4), List(5,6,7,8))
  }

  test("listLength function"){
    val expected = 8
    val actual = List(1,2,3,4,5,6,7,8)
  }

  test("elemOnPos function"){
    val expected = 4
    val actual = List(1,2,3,4).elemOnPos(3)
  }

  test("transpose function (2 lists)"){
    val expected = List(List(1,5),List(2,6),List(3,7),List(4,8))
    val lists = List(List(1,2,3,4), List(5,6,7,8))
    val actual = lists.transpose(lists)
  }

  test("transpose function (3 lists)"){
    val expected = List(List("a", "d", "g"),List("b","e","h"),List("c","f","i"))
    val lists = List(List("a","b","c"), List("d","e","f"), List("g","h","i"))
    val actual = lists.transpose(lists)
  }

  test("transpose function (4 lists)"){
    val expected = List(List(1,5,9,13),List(2,6,10,14),List(3,7,11,15),List(4,8,12,16))
    val lists = List(List(1,2,3,4), List(5,6,7,8), List(9,10,11,12), List(13,14,15,16))
    val actual = lists.transpose(lists)
  }
}
