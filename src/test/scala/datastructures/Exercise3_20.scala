package com.mattrjacobs.fp

import org.specs2._

class Exercise3_20 extends Specification {
  def is =
    "Exercise3-20" ^
      "List.filter" ^
      "empty" ! emptyFilter ^
      "nonEmptyOdd" ! nonEmptyFilterOdd ^
      "nonEmptyEven" ! nonEmptyFilterEven

  val l = Cons(1, Cons(2, Cons(3, Nil)))

  def emptyFilter = List.filter(Nil: List[Int])(_ % 2 == 1) must_== Nil
  def nonEmptyFilterOdd = List.filter(l)(_ % 2 == 1) must_== Cons(1, Cons(3, Nil))
  def nonEmptyFilterEven = List.filter(l)(_ % 2 == 0) must_== Cons(2, Nil)
}
