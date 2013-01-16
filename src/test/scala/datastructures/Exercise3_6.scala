package com.mattrjacobs.fp

import org.specs2._

class Exercise3_6 extends Specification {
  def is =
    "Exercise3-6" ^
      "List.init empty" ! emptyInit ^
      "List.init non-empty" ! nonEmptyInit

  val l = Cons("a", Cons("b", Cons("c", Nil)))

  def emptyInit = List.init(Nil) must_== Nil
  def nonEmptyInit = List.init(l) must_== Cons("a", Cons("b", Nil))
}
