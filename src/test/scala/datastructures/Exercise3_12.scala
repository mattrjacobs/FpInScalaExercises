package com.mattrjacobs.fp

import org.specs2._

class Exercise3_12 extends Specification {
  def is =
    "Exercise3-12" ^
      "List.length3" ^
      "empty" ! emptyLength ^
      "nonEmpty" ! nonEmptyLength ^
      p ^
      "List.sum3" ^
      "empty" ! emptySum ^
      "nonEmpty" ! nonEmptySum ^
      p ^
      "List.product3" ^
      "empty" ! emptyProduct ^
      "nonEmpty" ! nonEmptyProduct

  val l = Cons(1, Cons(2, Cons(3, Nil)))

  def emptyLength = List.length3(Nil) must_== 0
  def nonEmptyLength = List.length3(l) must_== 3

  def emptySum = List.sum3(Nil) must_== 0
  def nonEmptySum = List.sum3(l) must_== 6

  def emptyProduct = List.product3(Nil) must_== 1
  def nonEmptyProduct = List.product3(l) must_== 6
}
