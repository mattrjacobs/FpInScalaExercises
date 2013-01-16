package com.mattrjacobs.fp

import org.specs2._

class Exercise3_2 extends Specification {
  def is =
    "Exercise3-2" ^
      "List.tail empty" ! emptyTail ^
      "List.tail non-empty" ! nonEmptyTail

  val l = Cons("a", Cons("b", Cons("c", Nil)))

  def emptyTail = List.tail(Nil) must_== Nil
  def nonEmptyTail = List.tail(l) must_== Cons("b", Cons("c", Nil))
}
