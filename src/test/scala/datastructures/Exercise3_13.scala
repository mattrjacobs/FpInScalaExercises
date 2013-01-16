package com.mattrjacobs.fp

import org.specs2._

class Exercise3_13 extends Specification {
  def is =
    "Exercise3-13" ^
      "List.reverse empty" ! emptyReverse ^
      "List.reverse nonEmpty" ! nonEmptyReverse

  val l = Cons(1, Cons(2, Cons(3, Nil)))

  def emptyReverse = List.reverse(Nil) must_== Nil
  def nonEmptyReverse = List.reverse(l) must_== Cons(3, Cons(2, Cons(1, Nil)))
}
