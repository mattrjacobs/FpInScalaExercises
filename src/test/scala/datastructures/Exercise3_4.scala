package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_4 extends Specification {
  def is =
    "Exercise3-4" ^
      "Nil dropWhile" ! dropWhileNil ^
      "list dropWhile odd" ! dropWhileOdd ^
      "list dropWhile lessThan 4" ! dropWhileLessThanFour

  val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

  def dropWhileNil = List.dropWhile(Nil)(_ => true) must_== Nil
  def dropWhileOdd = List.dropWhile(l)(i => i % 2 == 1) must_== Cons(2, Cons(3, Cons(4, Nil)))
  def dropWhileLessThanFour = List.dropWhile(l)(i => i < 4) must_== Cons(4, Nil)
}
