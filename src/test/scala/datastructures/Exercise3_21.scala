package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_21 extends Specification {
  def is =
    "Exercise3-21" ^
      "List.flatMap" ^
      "empty" ! emptyFlatMap ^
      "nonEmpty" ! nonEmptyFlatMap

  val l = Cons(1, Cons(2, Cons(3, Nil)))

  def emptyFlatMap = List.flatMap(Nil)(i => Cons(i, Cons(i, Nil))) must_== Nil
  def nonEmptyFlatMap = List.flatMap(l)(i => Cons(i, Cons(i, Nil))) must_==
    Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Nil))))))
}
