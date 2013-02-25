package com.mattrjacobs.fp

import org.specs2._

class Exercise4_6 extends Specification {
  def is =
    "Exercise 4-6" ^
      "Option.traverse empty" ! emptyTraverse ^
      "Option.traverse all Nones" ! nonesTraverse ^
      "Option.traverse some Nones" ! someNonesTraverse ^
      "Option.traverse all Somes" ! somesTraverse

  def evenSome(i: Int) = i match {
    case i if i % 2 == 0 => Some(i)
    case _               => None
  }

  def emptyTraverse = Option.traverse(Nil)(evenSome) must_== Some(Nil)
  def nonesTraverse = Option.traverse(List(1, 3, 5))(evenSome) must_== None
  def someNonesTraverse = Option.traverse(List(1, 2, 3))(evenSome) must_== None
  def somesTraverse = Option.traverse(List(2, 4, 6))(evenSome) must_== Some(List(2, 4, 6))
}
