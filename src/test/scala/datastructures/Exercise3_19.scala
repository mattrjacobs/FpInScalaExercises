package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_19 extends Specification {
  def is =
    "Exercise3-19" ^
      "List.map" ^
      "empty" ! emptyMap ^
      "nonEmpty" ! nonEmptyMap

  val l = Cons(1, Cons(2, Cons(3, Nil)))

  def emptyMap = List.map(Nil: List[Int])(_ + 5) must_== Nil
  def nonEmptyMap = List.map(l)(_ + 5) must_== Cons(6, Cons(7, Cons(8, Nil)))
}
