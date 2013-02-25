package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_18 extends Specification {
  def is =
    "Exercise3-18" ^
      "List.doubleToString" ^
      "empty" ! emptyDoubleToString ^
      "nonEmpty" ! nonEmptyDoubleToString

  val l = Cons(1.0, Cons(0.2, Cons(3.5, Nil)))

  def emptyDoubleToString = List.doubleToString(Nil) must_== Nil
  def nonEmptyDoubleToString = List.doubleToString(l) must_== Cons("1.0", Cons("0.2", Cons("3.5", Nil)))
}
