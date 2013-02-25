package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_3 extends Specification {
  def is =
    "Exercise3-3" ^
      "Nil drop" ! dropNil ^
      "list drop 0" ! dropZero ^
      "list drop 2" ! dropTwo

  val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

  def dropNil = List.drop(Nil, 2) must_== Nil
  def dropZero = List.drop(l, 0) must_== l
  def dropTwo = List.drop(l, 2) must_== Cons(3, Cons(4, Nil))
}
