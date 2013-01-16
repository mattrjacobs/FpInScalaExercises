package com.mattrjacobs.fp

import org.specs2._

class Exercise3_5 extends Specification {
  def is =
    "Exercise3-5" ^
      "List.setHead empty" ! emptySetHead ^
      "List.setHead non-empty" ! nonEmptySetHead

  val l = Cons("a", Cons("b", Cons("c", Nil)))

  def emptySetHead = List.setHead(Nil)("d") must_== Nil
  def nonEmptySetHead = List.setHead(l)("d") must_== Cons("d", Cons("b", Cons("c", Nil)))
}
