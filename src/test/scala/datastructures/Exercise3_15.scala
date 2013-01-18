package com.mattrjacobs.fp

import org.specs2._

class Exercise3_15 extends Specification {
  def is =
    "Exercise3-15" ^
      "List.append first empty" ! firstEmpty ^
      "List.append second empty" ! secondEmpty ^
      "List.append neither empty" ! neitherEmpty

  val l1 = Cons(1, Cons(2, Cons(3, Nil)))
  val l2 = Cons(4, Cons(5, Nil))

  def firstEmpty = List.append2(Nil, l2) must_== l2
  def secondEmpty = List.append2(l1, Nil) must_== l1
  def neitherEmpty = List.append2(l1, l2) must_== Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
}
