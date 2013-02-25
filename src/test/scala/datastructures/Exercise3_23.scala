package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_23 extends Specification {
  def is =
    "Exercise3-23" ^
      "List.zip" ^
      "bothEmpty" ! bothEmptyZip ^
      "bothNonEmpty" ! nonEmptyZip

  val l1 = Cons(1, Cons(2, Cons(3, Nil)))
  val l2 = Cons(4, Cons(5, Cons(6, Nil)))

  def bothEmptyZip = List.zip(Nil: List[Int], Nil: List[Int])(_ + _) must_== Nil
  def nonEmptyZip = List.zip(l1, l2)(_ + _) must_== Cons(5, Cons(7, Cons(9, Nil)))
}
