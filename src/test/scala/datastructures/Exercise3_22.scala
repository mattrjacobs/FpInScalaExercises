package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_22 extends Specification {
  def is =
    "Exercise3-22" ^
      "List.filterInFlatMap" ^
      "empty" ! emptyFilterInFlatMap ^
      "nonEmptyOdd" ! nonEmptyFilterInFlatMapOdd ^
      "nonEmptyEven" ! nonEmptyFilterInFlatMapEven

  val l = Cons(1, Cons(2, Cons(3, Nil)))

  def emptyFilterInFlatMap = List.filterInFlatMap(Nil: List[Int])(_ % 2 == 1) must_== Nil
  def nonEmptyFilterInFlatMapOdd = List.filterInFlatMap(l)(_ % 2 == 1) must_== Cons(1, Cons(3, Nil))
  def nonEmptyFilterInFlatMapEven = List.filterInFlatMap(l)(_ % 2 == 0) must_== Cons(2, Nil)
}
