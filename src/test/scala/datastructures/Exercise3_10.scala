package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_10 extends Specification {
  def is =
    "Exercise3-10" ^
      "List.length empty" ! emptyLength ^
      "List.length nonEmpty" ! nonEmptyLength

  val l = Cons(1, Cons(2, Cons(3, Nil)))

  def emptyLength = List.length(Nil) must_== 0
  def nonEmptyLength = List.length(l) must_== 3
}
