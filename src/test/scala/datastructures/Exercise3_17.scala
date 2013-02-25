package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_17 extends Specification {
  def is =
    "Exercise3-17" ^
      "List.addOne" ^
      "empty" ! emptyAddOne ^
      "nonEmpty" ! nonEmptyAddOne

  val l = Cons(1, Cons(2, Cons(3, Nil)))

  def emptyAddOne = List.addOne(Nil) must_== Nil
  def nonEmptyAddOne = List.addOne(l) must_== Cons(2, Cons(3, Cons(4, Nil)))
}
